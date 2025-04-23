namespace SpellChess.Chess
    

// int alphaBeta( int alpha, int beta, int depthleft ) {
//    if( depthleft == 0 ) return quiesce( alpha, beta );
//    bestValue = -infinity;
//    for ( all moves)  {
//       score = -alphaBeta( -beta, -alpha, depthleft - 1 );
//       if( score > bestValue )
//       {
//          bestValue = score;
//          if( score > alpha )
//             alpha = score; // alpha acts like max in MiniMax
//       }
//       if( score >= beta )
//          return bestValue;   //  fail soft beta-cutoff, existing the loop here is also fine
//    }
//    return bestValue;
// }