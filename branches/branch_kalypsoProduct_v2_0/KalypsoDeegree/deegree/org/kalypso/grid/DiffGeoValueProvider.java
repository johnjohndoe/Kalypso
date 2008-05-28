package org.kalypso.grid;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * A {@link IGeoValueProvider} implementation which computes the difference of two given {@link IGeoValueProvider}s.
 * 
 * @author Gernot Belger
 */
public class DiffGeoValueProvider implements IGeoValueProvider
{
  private final IGeoValueProvider m_lower;

  private final IGeoValueProvider m_higher;

  /**
   * Creates an instance of this provider. Every value of this provider will be calculated a <code>higher</code>-<code>lower</code>.
   */
  public DiffGeoValueProvider( final IGeoValueProvider lower, final IGeoValueProvider higher )
  {
    m_lower = lower;
    m_higher = higher;
  }

  /**
   * @see com.bce.gis.operation.hmo2fli.DoubleProvider#getDouble(com.vividsolutions.jts.geom.Coordinate)
   */
  public final double getValue( final Coordinate crd ) throws GeoGridException
  {
    return m_higher.getValue( crd ) - m_lower.getValue( crd );
  }
}