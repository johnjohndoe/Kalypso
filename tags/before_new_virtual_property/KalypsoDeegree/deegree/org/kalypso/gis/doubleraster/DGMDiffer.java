package org.kalypso.gis.doubleraster;

import org.kalypso.gis.doubleraster.DoubleProvider;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * <p>
 * Dient zum eigentlichen Vergleichen zweier Höhenmodelle.
 * </p>
 * <p>
 * Die Höhendifferenz wird anhand eines {@link com.bce.gis.operation.hmo2fli.DoubleProvider} eingeteilt
 * </p>
 * 
 * @author belger
 */
public class DGMDiffer implements DoubleProvider
{
  private final DoubleProvider m_dpUnten;

  private final DoubleProvider m_dpOben;

  public DGMDiffer( final DoubleProvider dpUnten, final DoubleProvider dpOben )
  {
    m_dpUnten = dpUnten;
    m_dpOben = dpOben;
  }

  /**
   * @see com.bce.gis.operation.hmo2fli.DoubleProvider#getDouble(com.vividsolutions.jts.geom.Coordinate)
   */
  public final double getDouble( final Coordinate crd )
  {
    return m_dpOben.getDouble( crd ) - m_dpUnten.getDouble( crd );
  }

}
