package org.kalypso.psiadapter;

import org.kalypso.ogc.sensor.impl.DefaultAxis;

/**
 * Ein PSICompact Axis
 * 
 * @author schlienger
 */
public class PSICompactAxis extends DefaultAxis
{
  public final static int TYPE_DATE = 0;
  public final static int TYPE_VALUE = 1;
  public final static int TYPE_STATUS = 2;

  /**
   * Constructor
   * 
   * @param type eine von TYPE_* (Siehe diese Klasse Definition)
   */
  public PSICompactAxis( final String label, final String unit, final Class dataClass, final boolean restricted, final int type )
  {
    super( label, unit, dataClass, restricted, type );
  }
}
