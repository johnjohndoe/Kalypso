package org.kalypso.kalypsosimulationmodel.core.roughness;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.modeling.IColorStyledFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogRoughness;

/**
 * Interface for klasses representing the roughness feature of the type wbr:Roughness
 * 
 * @author Patrice Congo
 * 
 */
public interface IRoughnessCls extends IColorStyledFeatureWrapper
{
  public static final QName QNAME = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "RoughnessCls" ); //$NON-NLS-1$

  public static enum RoughnessConfigConsistency
  {
    ILLEGAL_VALUE_KS,
    ILLEGAL_VALUE_DP,
    ILLEGAL_VALUE_EDDY_XX,
    ILLEGAL_VALUE_EDDY_YX,
    ILLEGAL_VALUE_EDDY_XY,
    ILLEGAL_VALUE_EDDY_YY,
    ILLEGAL_VALUE_AXAY,
    ILLEGAL_VALUE_MARSH,
    ILLEGAL_VALUE_NAME,
    ILLEGAL_VALUE_URI,
    URI_IN_WORKSPACE,
    OK
  }

  /**
   * To get the roughness ks value
   * 
   * @return the roughness kx value
   */
  public double getKs( );

  /**
   * To sets the roughness ks value
   * 
   * @param ks
   *          -- the new ks value
   * @throws IllegalArgumentException
   */
  public void setKs( double ks ) throws IllegalArgumentException;

  /**
   * Gets vegetation parameter
   * 
   * @return returns the vegetation parameter
   */
  public double getAxAy( );

  /**
   * Sets the vegetation parameter
   * 
   * @param axay
   * @throws IllegalArgumentException
   */
  public void setAxAy( double axay ) throws IllegalArgumentException;

  public double getDp( );

  public void setDp( double dp ) throws IllegalArgumentException;

  public double getMarsh( );

  public void setMarsh( double marsh ) throws IllegalArgumentException;

  /**
   * To get the eddy viscosity of this roughness
   * 
   * @return the eddy viskosity of this roughness
   */
  public double getEddyXX( );

  public double getEddyYX( );

  public double getEddyXY( );

  public double getEddyYY( );

  /**
   * Sets a new eddy viscosity for this roughness
   * 
   * @param eddy
   *          the new eddy viscosity
   * 
   * @throws IllegalArgumentException
   *           if eddy is negative
   * 
   */
  public void setEddyXX( double eddy_xx ) throws IllegalArgumentException;

  public void setEddyYX( double eddy_yx ) throws IllegalArgumentException;

  public void setEddyXY( double eddy_xy ) throws IllegalArgumentException;

  public void setEddyYY( double eddy_yy ) throws IllegalArgumentException;

  /**
   * Configure this roughness with the given feature The configuration is only apply if the set of parameter passed the
   * valitidy check
   * 
   * @param name
   *          the name for the roughness
   * @param ks
   *          the Manning Strickler coefficient
   * @param axay
   * @param dp
   * @param eddy
   *          the eddy viskosity
   * @param marsh
   *          marsh parameter
   * @return
   */
  public RoughnessConfigConsistency configure( String name, double ks, double axay, double dp, double eddy_xx, double eddy_yx, double eddy_xy, double eddy_yy, double marsh );

  /**
   * To validates the current configuration of this roughness. This method is specially usefull after an independent
   * setting of the different parameter of the roughness
   * 
   * @return a {@link RoughnessConfigConsistency} hint the the actual roughness configuration
   */
  public RoughnessConfigConsistency validate( );

  public Double[] getViscosities( final int iedsw );
}
