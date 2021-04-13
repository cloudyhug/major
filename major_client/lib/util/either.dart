class Either<L, R> {
  final L? left;
  final R? right;

  Either._internal(this.left, this.right);
  factory Either.left(L value) => Either._internal(value, null);
  factory Either.right(R value) => Either._internal(null, value);

  bool isLeft() => left != null;
  bool isRight() => right != null;
}