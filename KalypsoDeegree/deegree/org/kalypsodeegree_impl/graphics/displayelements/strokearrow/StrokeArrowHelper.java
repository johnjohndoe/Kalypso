/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.graphics.displayelements.strokearrow;

import java.util.Map;

import org.apache.commons.lang.NotImplementedException;
import org.kalypsodeegree.graphics.sld.CssParameter;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.Stroke;

/**
 * @author kuch
 */
public class StrokeArrowHelper
{

  private static final String STROKE_WIDTH = "stroke-width";

  public enum ARROW_TYPE
  {
    eLine,
    eSegmentOfLine;

    public static ARROW_TYPE getType( CssParameter param )
    {
      ParameterValueType value = param.getValue();
      Object[] components = value.getComponents();
      if( components.length != 1 )
        throw (new IllegalStateException( "StrokeArrowType consists only of one parameter." ));

      String type = ((String) components[0]).toLowerCase();

      if( type.equals( "line" ) )
        return ARROW_TYPE.eLine;
      else if( type.equals( "segment" ) )
        return ARROW_TYPE.eSegmentOfLine;

      throw (new NotImplementedException());
    }
  }

  public enum ARROW_ALIGNMENT
  {
    eStart,
    eMiddle,
    eEnd;
    public static ARROW_ALIGNMENT getType( CssParameter param )
    {
      ParameterValueType value = param.getValue();
      Object[] components = value.getComponents();
      if( components.length != 1 )
        throw (new IllegalStateException( "StrokeArrowAligment can only consists about one parameter." ));

      String type = ((String) components[0]).toLowerCase();

      if( type.equals( "start" ) )
        return ARROW_ALIGNMENT.eStart;
      else if( type.equals( "middle" ) )
        return ARROW_ALIGNMENT.eMiddle;
      else if( type.equals( "end" ) )
        return ARROW_ALIGNMENT.eEnd;

      throw (new NotImplementedException());
    }
  }

  public static final String STROKE_ARROW_TYPE = "stroke-arrow-type";

  public static final String STROKE_ARROW_ALIGNMENT = "stroke-arrow-alignment";

  public static final String STROKE_ARROW_SIZE = "stroke-arrow-size";

  public static boolean isArrowDefined( Stroke stroke )
  {
    Map< ? , ? > map = stroke.getCssParameters();
    Object object = map.get( STROKE_ARROW_TYPE );

    if( object != null )
      return true;

    return false;
  }

  public static ARROW_TYPE getArrowType( Map< ? , ? > cssParameters )
  {
    CssParameter csType = (CssParameter) cssParameters.get( STROKE_ARROW_TYPE );

    return ARROW_TYPE.getType( csType );
  }

  public static ARROW_ALIGNMENT getArrowAlignment( Map< ? , ? > cssParameters )
  {
    CssParameter csType = (CssParameter) cssParameters.get( STROKE_ARROW_ALIGNMENT );

    return ARROW_ALIGNMENT.getType( csType );
  }

  public static Double getArrowSize( Map< ? , ? > cssParameters )
  {
    CssParameter csType = (CssParameter) cssParameters.get( STROKE_ARROW_SIZE );

    ParameterValueType value = csType.getValue();
    Object[] components = value.getComponents();
    if( components.length != 1 )
      throw (new IllegalStateException( "StrokeArrowAligment can only consists about one parameter." ));

    return Double.valueOf( (String) components[0] );
  }

  public static Double getStrokeWidth( Map< ? , ? > cssParameters )
  {
    CssParameter csType = (CssParameter) cssParameters.get( STROKE_WIDTH );
    if( csType == null )
      return 1.0;

    ParameterValueType value = csType.getValue();
    Object[] components = value.getComponents();
    if( components.length != 1 )
      throw (new IllegalStateException( "StrokeWidth can only consists about one parameter." ));

    return Double.valueOf( (String) components[0] );
  }
}
