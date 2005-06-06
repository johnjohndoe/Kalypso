package com.bce.eind.core.result;

import com.bce.eind.core.result.IResult.TYPE;
import com.bce.eind.core.result.impl.Result;

public class ResultFactory
{
  public static IResult createResult( final String name, final TYPE type )
  {
    return new Result( name, type );
  }

  public static IResult createResult( final String name, final TYPE type, final double[] stations,
      final double[] values )
  {
    if( stations.length != values.length )
      throw new IllegalArgumentException( "stations and values must be of same size." );

    final IResult result = createResult( name, type );

    for( int i = 0; i < values.length; i++ )
      result.addResult( stations[i], values[i] );

    return result;
  }
}
