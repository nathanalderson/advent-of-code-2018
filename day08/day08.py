from typing import List

def ans(data: List[int]) -> (int, int, List[int]):
    num_children = data.pop(0)
    num_meta = data.pop(0)
    values = []
    meta_total = 0
    for _ in range(num_children):
        total, value, data = ans(data)
        meta_total += total
        values.append(value)
    metas, rest = data[:num_meta], data[num_meta:]
    meta_total += sum(metas)
    if num_children == 0:
        value = sum(metas)
    else:
        value = sum(values[m-1] for m in metas if m > 0 and m <= len(values))
    return meta_total, value, rest

def main():
    with open("input.txt") as f:
        data = [int(s) for s in f.read().split(" ")]
    total, value, remaining = ans(data)
    print('part 1:', total)
    print('part 2:', value)

if __name__ == "__main__":
    main()
