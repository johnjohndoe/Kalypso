package org.kalypso.kalypsosimulationmodel.core.roughness;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogRoughness;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Interface for klasses representing the roughness feature of the type wbr:Roughness
 * 
 * @author Patrice Congo
 * 
 */
public interface IRoughnessClsCorrection extends Feature
{
  // //corrections
  public static final QName WBR_F_ROUGHNESS_CORRECTION = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "RoughnessClsCorrection" ); //$NON-NLS-1$

  public static final QName WBR_PROP_KS_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "ksCor" ); //$NON-NLS-1$

  public static final QName WBR_PROP_AXAY_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "axayCor" ); //$NON-NLS-1$

  public static final QName WBR_PROP_DP_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "dpCor" ); //$NON-NLS-1$

  public static final QName WBR_PROP_EDDY_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "eddyCor" ); //$NON-NLS-1$

  public static final QName WBR_PROP_MARSH_COR = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "marshCor" ); //$NON-NLS-1$
  
  public static enum RoughnessCorConfigConsistency
  {
    ILLEGAL_VALUE_KS_COR,
    ILLEGAL_VALUE_DP_COR,
    ILLEGAL_VALUE_EDDY_COR,
    ILLEGAL_VALUE_AXAY_COR,
    ILLEGAL_VALUE_MARSH_COR,
    ILLEGAL_VALUE_URI_COR,
    URI_IN_WORKSPACE_COR,
    OK
  }

  /**
   * To get the correction factor for the roughness ks value
   * 
   * @return the correction factor for ks value
   */
  public double getKsCor( );

  /**
   * To sets correction factor for the roughness ks value
   * 
   * @param ksCor --
   *            the new correctionfactor for ks value
   * @throws IllegalArgumentException
   */
  public void setKsCor( double ksCor ) throws IllegalArgumentException;

  /**
   * Gets correction factor for vegetation parameter axay
   * 
   * @return returns the correction factor for vegetation parameter axay
   */
  public double getAxAyCor( );

  public double getMarshCor( );

  public void setMarshCor( double marsh );

  /**
   * Sets the correction factor for vegetation parameter axay
   * 
   * @param axayCor
   *            the new correction factor for the vegetation parameter axay
   * @throws IllegalArgumentException
   */
  public void setAxAyCor( double axayCor ) throws IllegalArgumentException;

  /**
   * Get the correction factor for vegetation parameter DP
   * 
   * @return the correction parameter for vegetation parameter DP
   */
  public double getDpCor( );

  /**
   * Set the correction parameter for vegetation factor dp
   * 
   * @param dpCor
   *            the new correction parameter for vegetation dp
   * 
   * @throws IllegalArgumentException
   *             if dp is negativ
   */
  public void setDpCor( double dp ) throws IllegalArgumentException;

  /**
   * To get correction parameter fo the eddy viscosity
   * 
   * @return the correction parameter for the eddy viskosity
   */
  public double getEddyCor( );

  /**
   * Sets a new correction parameter for the eddy viscosity
   * 
   * @param eddyCor
   *            the new correction parameter for the eddy viscosity
   * 
   * @throws IllegalArgumentException
   *             if correction parameter is negative
   * 
   */
  public void setEddyCor( double eddyCor ) throws IllegalArgumentException;

  /**
   * Configure this roughness correction with all the correction factors. The configuration is only applied if the set
   * of parameter passed the valitidy check
   * 
   * @param name
   *            the name for the roughness
   * @param ksCor
   *            correction factor for ks value
   * @param axayCor
   *            correction factor for axay value
   * @param dpCor
   *            correction factor for dp value
   * @param eddyCor
   *            correction factor for the eddy viskosity
   * @param marshCor
   *            correction parameter for configure
   * 
   * @return {@link RoughnessCorConfigConsistency#OK} if the operation succed otherwise a corresponding consistency
   *         check key
   * 
   */
  public RoughnessCorConfigConsistency configure( double ksCor, double axayCor, double dpCor, double eddyCor, double marshcor );

  /**
   * To validates the current configuration of this roughness. This method is specially usefull after an independent
   * setting of the different parameter of the roughness
   * 
   * @return a {@link RoughnessCorConfigConsistency} hint the the actual roughness configuration
   */
  public RoughnessCorConfigConsistency validate( );
}
