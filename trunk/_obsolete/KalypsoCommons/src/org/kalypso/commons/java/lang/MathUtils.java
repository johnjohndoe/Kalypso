/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.commons.java.lang;

/**
 * @author doemming,fichtner
 */
public class MathUtils
{
  /**
   * @return f(x) on line (x1,y1) (x2,y2)
   */
  public static double interpolate( long x1, long x2, double y1, double y2, long x )
  {
    return y1 + (y2 - y1) * (((double) (x - x1)) / ((double) (x2 - x1)));
  }

  public static enum RoundMethod
  {
    UP
    {
      @Override
      public double round( final double d )
      {
        return Math.ceil( d );
      }
    },
    DOWN
    {
      @Override
      public double round( final double d )
      {
        return Math.floor( d );
      }
    },
    HALF_UP
    {
      @Override
      public double round( final double d )
      {
        return Math.rint( d );
      }
    };

    public abstract double round( final double d );
  }

  public static double round( double d, final RoundMethod method )
  {
    int faktor = 1;
    boolean negative = false;

    if( d == 0 || Double.isInfinite( d ) )
      return d;

    if( d < 0 )
    {
      d = -d;
      negative = true;
    }

    if( d < 1 )
    {
      while( d < 1 )
      {
        d = d * 10;
        faktor = faktor * 10;
      }

      d = method.round( d );
      d = d / faktor;
    }
    else if( d > 10 )
    {
      while( d > 10 )
      {
        d = d / 10;
        faktor = faktor * 10;
      }

      d = method.round( d );

      d = d * faktor;
    }
    else
      d = method.round( d );

    return negative ? -d : d;
  }

  /**
   * Gibt die Anzahl der Stellen nach dem Komma bis zur ersten Zahl != 0
   */
  public static int scale( double d )
  {
    double abs = Math.abs( d );

    if( abs > 1 )
      return 0;

    double bruchTeil = abs - Math.floor( abs );
    int scale = 0;

    if( bruchTeil > 0 )
    {
      while( bruchTeil < 1 )
      {
        bruchTeil = bruchTeil * 10;
        scale++;
      }
    }

    return scale;
  }

  /**
   * Rundet die Zahl d auf die Position scale mit einer bestimmten Rundungsmethode roundMethod
   */
  public static double setScale( double d, final int scale, final RoundMethod method )
  {
    int c = 0;
    long faktor = 1;

    while( c < scale )
    {
      d = d * 10;
      faktor = faktor * 10;
      c++;
    }

    d = method.round( d );

    // den Originalwert wiederherstellen
    d = d / faktor;

    return d;
  }

  public static final double nanMin( final double v1, final double v2 )
  {
    if( Double.isNaN( v1 ) )
      return v2;

    if( Double.isNaN( v2 ) )
      return v1;

    return Math.min( v1, v2 );
  }

  public static final double nanMax( final double v1, final double v2 )
  {
    if( Double.isNaN( v1 ) )
      return v2;

    if( Double.isNaN( v2 ) )
      return v1;

    return Math.max( v1, v2 );
  }
}
