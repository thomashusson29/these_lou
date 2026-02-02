#!/usr/bin/env python3
import argparse
import csv
import re
import unicodedata
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple


def strip_accents(text: str) -> str:
    return "".join(
        ch for ch in unicodedata.normalize("NFKD", text) if not unicodedata.combining(ch)
    )


_RE_WS = re.compile(r"\s+")


def normalize_age(age: str) -> str:
    age = (age or "").strip().lower().replace("\u00a0", " ")
    age = strip_accents(age)
    age = re.sub(r"[^a-z0-9]+", " ", age)
    age = _RE_WS.sub(" ", age).strip()
    return age


def normalize_city(city: str) -> str:
    city = (city or "").strip().lower().replace("\u00a0", " ")
    city = strip_accents(city)
    city = re.sub(r"\bst[\s\.-]+", "saint ", city)
    city = re.sub(r"\bste[\s\.-]+", "sainte ", city)
    city = re.sub(r"[^a-z0-9]+", " ", city)
    city = _RE_WS.sub(" ", city).strip()
    city = re.sub(r"^(le|la|les|l)\s+", "", city)
    parts = city.split()
    if len(parts) >= 2 and len(parts[-1]) == 1 and parts[-1].isalpha():
        city = " ".join(parts[:-1])
    return city


def parse_score(value: str) -> Optional[float]:
    value = (value or "").strip()
    if not value:
        return None
    value = value.replace(",", ".")
    match = re.search(r"-?\d+(?:\.\d+)?", value)
    if not match:
        return None
    try:
        return float(match.group(0))
    except ValueError:
        return None


def score_for_matching(source: str, score: Optional[float]) -> Optional[float]:
    if score is None:
        return None
    if source == "Q1" and score > 20:
        return score / 2.0
    return score


def read_csv(path: Path, *, encoding: str, delimiter: str = ";") -> Tuple[List[str], List[List[str]]]:
    with path.open("r", encoding=encoding, newline="") as f:
        reader = csv.reader(f, delimiter=delimiter)
        header = next(reader)
        rows: List[List[str]] = []
        for row in reader:
            if len(row) < len(header):
                row = row + [""] * (len(header) - len(row))
            elif len(row) > len(header):
                row = row[: len(header)]
            rows.append(row)
    header = [h.replace("\r", " ").replace("\n", " ").strip() for h in header]
    return header, rows


def read_csv_auto(path: Path, *, delimiter: str = ";", encodings: Sequence[str]) -> Tuple[List[str], List[List[str]]]:
    last_err: Optional[Exception] = None
    for enc in encodings:
        try:
            return read_csv(path, encoding=enc, delimiter=delimiter)
        except Exception as exc:  # noqa: BLE001
            last_err = exc
            continue
    raise RuntimeError(f"Impossible de lire {path} avec les encodages {encodings}") from last_err


def normalize_header(text: str) -> str:
    text = (text or "").strip().lower().replace("\u00a0", " ")
    text = strip_accents(text)
    text = re.sub(r"[^a-z0-9]+", " ", text)
    text = _RE_WS.sub(" ", text).strip()
    return text


def find_col_index(header: Sequence[str], *, contains_all: Sequence[str]) -> int:
    """
    Find first column index where normalized header contains all tokens.
    Tokens are compared on normalized strings.
    """
    tokens = [normalize_header(t) for t in contains_all]
    for i, h in enumerate(header):
        nh = normalize_header(h)
        if all(t in nh for t in tokens):
            return i
    raise KeyError(f"Colonne introuvable (contient: {contains_all}) dans l'en-tête: {header}")


def hungarian_min_cost(cost: Sequence[Sequence[float]]) -> List[int]:
    """
    Hungarian algorithm (minimization) for a rectangular matrix with n <= m.
    Returns assignment list of length n: assigned column index (0..m-1) for each row.
    """
    n = len(cost)
    if n == 0:
        return []
    m = len(cost[0])
    if any(len(row) != m for row in cost):
        raise ValueError("Cost matrix must be rectangular")
    if n > m:
        raise ValueError("Hungarian requires n <= m (transpose or pad first)")

    # 1-indexed implementation (standard competitive programming version)
    u = [0.0] * (n + 1)
    v = [0.0] * (m + 1)
    p = [0] * (m + 1)
    way = [0] * (m + 1)

    for i in range(1, n + 1):
        p[0] = i
        j0 = 0
        minv = [float("inf")] * (m + 1)
        used = [False] * (m + 1)
        while True:
            used[j0] = True
            i0 = p[j0]
            delta = float("inf")
            j1 = 0
            for j in range(1, m + 1):
                if used[j]:
                    continue
                cur = cost[i0 - 1][j - 1] - u[i0] - v[j]
                if cur < minv[j]:
                    minv[j] = cur
                    way[j] = j0
                if minv[j] < delta:
                    delta = minv[j]
                    j1 = j
            for j in range(0, m + 1):
                if used[j]:
                    u[p[j]] += delta
                    v[j] -= delta
                else:
                    minv[j] -= delta
            j0 = j1
            if p[j0] == 0:
                break
        while True:
            j1 = way[j0]
            p[j0] = p[j1]
            j0 = j1
            if j0 == 0:
                break

    # p[j] = assigned row for column j
    assignment = [-1] * n
    for j in range(1, m + 1):
        if p[j] != 0:
            assignment[p[j] - 1] = j - 1
    if any(a < 0 for a in assignment):
        raise RuntimeError("Assignment incomplete")
    return assignment


@dataclass(frozen=True)
class Record:
    source: str  # "Q1" or "Q2"
    row_number: int  # 1-based within file (excluding header)
    age_raw: str
    city_raw: str
    age_key: str
    city_key: str
    score_raw: str
    score: Optional[float]
    score_match: Optional[float]
    values_by_header: Dict[str, str]


def build_records(
    *,
    source: str,
    header: List[str],
    rows: List[List[str]],
    age_col: int,
    city_col: int,
    score_col: int,
) -> List[Record]:
    out: List[Record] = []
    for idx, row in enumerate(rows, start=1):
        values = {header[i]: row[i] for i in range(len(header))}
        age_raw = row[age_col]
        city_raw = row[city_col]
        score_raw = row[score_col]
        score = parse_score(score_raw)
        out.append(
            Record(
                source=source,
                row_number=idx,
                age_raw=age_raw,
                city_raw=city_raw,
                age_key=normalize_age(age_raw),
                city_key=normalize_city(city_raw),
                score_raw=score_raw,
                score=score,
                score_match=score_for_matching(source, score),
                values_by_header=values,
            )
        )
    return out


def match_group(
    q1_indices: List[int],
    q2_indices: List[int],
    q1: List[Record],
    q2: List[Record],
) -> Dict[int, int]:
    """
    Returns mapping Q1_index -> Q2_index for matched pairs within a key group.
    Always matches min(len(q1_indices), len(q2_indices)) pairs.

    Objective (lexicographic):
      1) maximize count(score2 - score1 >= 0)
      2) maximize sum(score2 - score1)
    """
    if not q1_indices or not q2_indices:
        return {}

    # Deterministic ordering
    q1_indices = sorted(q1_indices, key=lambda i: q1[i].row_number)
    q2_indices = sorted(q2_indices, key=lambda i: q2[i].row_number)

    # Make rows the smaller side for Hungarian (n <= m)
    transpose = False
    left = q1_indices
    right = q2_indices
    if len(left) > len(right):
        transpose = True
        left, right = right, left  # type: ignore[assignment]

    n_pairs = min(len(q1_indices), len(q2_indices))

    diffs: List[float] = []
    for li in left:
        for rj in right:
            rec_left = q1[li] if not transpose else q2[li]
            rec_right = q2[rj] if not transpose else q1[rj]
            s1 = rec_left.score_match
            s2 = rec_right.score_match
            diff = (s2 - s1) if (s1 is not None and s2 is not None) else 0.0
            diffs.append(abs(diff))
    max_abs_diff = max(diffs) if diffs else 1.0
    bonus = (n_pairs + 1) * (max_abs_diff + 1.0) + 1.0

    # Build cost matrix for minimization
    cost_matrix: List[List[float]] = []
    for li in left:
        row_cost: List[float] = []
        for rj in right:
            rec_left = q1[li] if not transpose else q2[li]
            rec_right = q2[rj] if not transpose else q1[rj]
            s1 = rec_left.score_match
            s2 = rec_right.score_match
            diff = (s2 - s1) if (s1 is not None and s2 is not None) else 0.0
            improved = 1.0 if diff >= 0 else 0.0
            weight = bonus * improved + diff
            row_cost.append(-weight)
        cost_matrix.append(row_cost)

    assignment = hungarian_min_cost(cost_matrix)

    # Build Q1->Q2 mapping (only up to n_pairs)
    mapping: Dict[int, int] = {}
    used_right = set()
    for li_pos, rj_pos in enumerate(assignment):
        if len(mapping) >= n_pairs:
            break
        li = left[li_pos]
        rj = right[rj_pos]
        if rj in used_right:
            continue
        used_right.add(rj)
        if not transpose:
            mapping[li] = rj
        else:
            # left is Q2, right is Q1; invert
            mapping[rj] = li
    return mapping


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Apparie Q1 et Q2 par (tranche d’âge, ville) et génère un CSV fusionné."
    )
    parser.add_argument("--q1", default="Q1.csv", help="Chemin vers Q1.csv")
    parser.add_argument("--q2", default="Q2.csv", help="Chemin vers Q2.csv")
    parser.add_argument(
        "--out",
        default="Q1_Q2_apparie.csv",
        help="Chemin du CSV de sortie (UTF-8 avec BOM, séparateur ;)",
    )
    parser.add_argument(
        "--resume",
        default="Q1_Q2_apparie_resume.txt",
        help="Chemin du fichier résumé",
    )
    args = parser.parse_args()

    q1_path = Path(args.q1)
    q2_path = Path(args.q2)

    q1_header, q1_rows = read_csv_auto(
        q1_path,
        delimiter=";",
        encodings=("utf-8-sig", "utf-8", "mac_roman"),
    )
    q2_header, q2_rows = read_csv_auto(
        q2_path,
        delimiter=";",
        encodings=("utf-8-sig", "utf-8", "mac_roman"),
    )

    # Small header fix for Q2 first column
    if q2_header and q2_header[0] == "uestio":
        q2_header = ["Horodatage"] + q2_header[1:]
        q2_rows = [([row[0]] + row[1:]) for row in q2_rows]

    # Column indices (auto-detection from headers)
    q1_age_col = find_col_index(q1_header, contains_all=("age",))
    q1_city_col = find_col_index(q1_header, contains_all=("ville", "habitez"))
    q1_score_col = find_col_index(q1_header, contains_all=("note",))

    q2_age_col = find_col_index(q2_header, contains_all=("age",))
    q2_city_col = find_col_index(q2_header, contains_all=("ville", "habitez"))
    q2_score_col = find_col_index(q2_header, contains_all=("note",))

    q1_records = build_records(
        source="Q1",
        header=q1_header,
        rows=q1_rows,
        age_col=q1_age_col,
        city_col=q1_city_col,
        score_col=q1_score_col,
    )
    q2_records = build_records(
        source="Q2",
        header=q2_header,
        rows=q2_rows,
        age_col=q2_age_col,
        city_col=q2_city_col,
        score_col=q2_score_col,
    )

    groups_q1: Dict[Tuple[str, str], List[int]] = defaultdict(list)
    groups_q2: Dict[Tuple[str, str], List[int]] = defaultdict(list)
    for i, rec in enumerate(q1_records):
        groups_q1[(rec.age_key, rec.city_key)].append(i)
    for i, rec in enumerate(q2_records):
        groups_q2[(rec.age_key, rec.city_key)].append(i)

    all_keys = sorted(set(groups_q1) | set(groups_q2))

    q1_to_q2: Dict[int, int] = {}
    matched_by_age_only: set[int] = set()

    # Pass 1: strict match on (age, city)
    for key in all_keys:
        q1_idxs = groups_q1.get(key, [])
        q2_idxs = groups_q2.get(key, [])
        if not q1_idxs or not q2_idxs:
            continue
        q1_to_q2.update(match_group(q1_idxs, q2_idxs, q1_records, q2_records))

    # Pass 2: fallback match on age only for remaining (still 1-1)
    used_q2 = set(q1_to_q2.values())
    remaining_q1 = [i for i in range(len(q1_records)) if i not in q1_to_q2]
    remaining_q2 = [j for j in range(len(q2_records)) if j not in used_q2]

    by_age_q1: Dict[str, List[int]] = defaultdict(list)
    by_age_q2: Dict[str, List[int]] = defaultdict(list)
    for i in remaining_q1:
        by_age_q1[q1_records[i].age_key].append(i)
    for j in remaining_q2:
        by_age_q2[q2_records[j].age_key].append(j)

    for age_key in sorted(set(by_age_q1) & set(by_age_q2)):
        new_pairs = match_group(by_age_q1[age_key], by_age_q2[age_key], q1_records, q2_records)
        for qi, qj in new_pairs.items():
            if qi in q1_to_q2:
                continue
            if qj in used_q2:
                continue
            q1_to_q2[qi] = qj
            used_q2.add(qj)
            matched_by_age_only.add(qi)

    # Build output
    base_fields = [
        "match_id",
        "age_key",
        "city_key",
        "match_type",
        "Q1_city_key",
        "Q2_city_key",
        "Q1_row",
        "Q2_row",
        "Q1_score_parsed",
        "Q2_score_parsed",
        "score_diff_parsed",
        "Q1_score_utilisee",
        "Q2_score_utilisee",
        "score_diff_utilisee",
        "amelioree",
    ]
    q1_pref = [f"Q1__{h}" for h in q1_header]
    q2_pref = [f"Q2__{h}" for h in q2_header]
    out_header = base_fields + q1_pref + q2_pref

    out_rows: List[List[str]] = []
    match_id = 0
    matched_q2 = set()
    for i, rec1 in enumerate(q1_records):
        rec2 = None
        if i in q1_to_q2:
            rec2 = q2_records[q1_to_q2[i]]
            matched_q2.add(q1_to_q2[i])
            match_id += 1

        match_type = ""
        if rec2:
            match_type = "age" if i in matched_by_age_only else "age+ville"

        s1 = rec1.score
        s2 = rec2.score if rec2 else None
        diff_parsed = (s2 - s1) if (s1 is not None and s2 is not None) else None

        s1m = rec1.score_match
        s2m = rec2.score_match if rec2 else None
        diff_used = (s2m - s1m) if (s1m is not None and s2m is not None) else None
        improved = 1 if (diff_used is not None and diff_used >= 0) else 0

        row: List[str] = [
            str(match_id) if rec2 else "",
            rec1.age_key,
            rec1.city_key if (rec2 and match_type == "age+ville") else "",
            match_type,
            rec1.city_key,
            rec2.city_key if rec2 else "",
            str(rec1.row_number),
            str(rec2.row_number) if rec2 else "",
            "" if s1 is None else f"{s1:.4g}",
            "" if s2 is None else f"{s2:.4g}",
            "" if diff_parsed is None else f"{diff_parsed:.4g}",
            "" if s1m is None else f"{s1m:.4g}",
            "" if s2m is None else f"{s2m:.4g}",
            "" if diff_used is None else f"{diff_used:.4g}",
            str(improved) if rec2 else "",
        ]

        row.extend(rec1.values_by_header.get(h, "") for h in q1_header)
        if rec2:
            row.extend(rec2.values_by_header.get(h, "") for h in q2_header)
        else:
            row.extend([""] * len(q2_header))
        out_rows.append(row)

    out_path = Path(args.out)
    with out_path.open("w", encoding="utf-8-sig", newline="") as f:
        w = csv.writer(f, delimiter=";", quoting=csv.QUOTE_MINIMAL)
        w.writerow(out_header)
        w.writerows(out_rows)

    # Summary
    n_q1 = len(q1_records)
    n_q2 = len(q2_records)
    n_pairs = len(q1_to_q2)
    unmatched_q1 = n_q1 - n_pairs
    age_only_pairs = len(matched_by_age_only)
    strict_pairs = n_pairs - age_only_pairs

    diffs_matched = []
    improved_count = 0
    for i, j in q1_to_q2.items():
        s1 = q1_records[i].score_match
        s2 = q2_records[j].score_match
        if s1 is None or s2 is None:
            continue
        d = s2 - s1
        diffs_matched.append(d)
        if d >= 0:
            improved_count += 1

    group_stats = []
    for key in set(groups_q1) | set(groups_q2):
        a = len(groups_q1.get(key, []))
        b = len(groups_q2.get(key, []))
        if a == 0 or b == 0:
            continue
        group_stats.append((min(a, b), a, b, key))
    group_stats.sort(reverse=True)

    q1_big_scores = [rec for rec in q1_records if rec.score is not None and rec.score > 20]

    resume_lines = []
    resume_lines.append(f"Q1: {n_q1} lignes")
    resume_lines.append(f"Q2: {n_q2} lignes")
    resume_lines.append(f"Paires (appariées): {n_pairs}")
    resume_lines.append(f"- dont âge+ville: {strict_pairs}")
    resume_lines.append(f"- dont âge seul (fallback): {age_only_pairs}")
    resume_lines.append(f"Q1 non appariées: {unmatched_q1}")
    if diffs_matched:
        resume_lines.append(f"Paires avec amélioration (Q2>=Q1): {improved_count}/{len(diffs_matched)}")
        avg = sum(diffs_matched) / len(diffs_matched)
        resume_lines.append(f"Gain moyen (Q2-Q1) sur paires scorées: {avg:.3g}")
        resume_lines.append(f"Gain min/max (Q2-Q1) sur paires scorées: {min(diffs_matched):.3g} / {max(diffs_matched):.3g}")
    else:
        resume_lines.append("Aucune paire avec scores exploitables pour calculer les gains.")

    resume_lines.append("")
    resume_lines.append("Top groupes par nombre de paires possibles (min(Q1,Q2)):")
    for k, a, b, key in group_stats[:15]:
        resume_lines.append(f"- {key[0]} | {key[1]} : Q1={a} Q2={b} (max paires={k})")

    # Keys present in both but with extra unmatched on Q1
    q1_unmatched_by_key = Counter()
    for key, q1_idxs in groups_q1.items():
        q2_idxs = groups_q2.get(key, [])
        if not q2_idxs:
            continue
        # count unmatched Q1 in that key
        used = sum(1 for i in q1_idxs if i in q1_to_q2)
        q1_unmatched_by_key[key] = len(q1_idxs) - used

    extra = [(cnt, key) for key, cnt in q1_unmatched_by_key.items() if cnt > 0]
    extra.sort(reverse=True)
    resume_lines.append("")
    resume_lines.append("Groupes avec Q1 non appariées (malgré présence Q2):")
    for cnt, key in extra[:15]:
        resume_lines.append(f"- {key[0]} | {key[1]} : Q1 en trop = {cnt}")

    if q1_big_scores:
        resume_lines.append("")
        resume_lines.append(
            f"Attention: {len(q1_big_scores)} lignes Q1 ont une note > 20 (ex: {q1_big_scores[0].score_raw!r}) et sont divisées par 2 pour l’optimisation."
        )

    Path(args.resume).write_text("\n".join(resume_lines) + "\n", encoding="utf-8")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
