package org.kalypso.model.wspm.core.gml;

import java.math.BigDecimal;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public class ProfileFeatureBinding extends Feature_Impl implements  IProfileFeature
{

  public ProfileFeatureBinding( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#getBigStation()
   */
  @Override
  public BigDecimal getBigStation( )
  {
    return (BigDecimal) getProperty( ProfileFeatureFactory.QNAME_STATION );
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#getLine()
   */
  @Override
  public GM_Curve getLine( )
  {
    return (GM_Curve) getFeature().getProperty( QNAME_LINE );
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#getProfil()
   */
  @Override
  public IProfil getProfil( )
  {
    try
    {
      return ProfileFeatureFactory.toProfile( this );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModelWspmCorePlugin.getDefault().getLog().log( status );

      return null;
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#getSrsName()
   */
  @Override
  public String getSrsName( )
  {
    return (String) getProperty( QNAME_SRS );
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#getStation()
   */
  @Override
  public double getStation( )
  {
    final BigDecimal profileStation = getBigStation();

    return profileStation == null ? Double.NaN : profileStation.doubleValue();
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#getWater()
   */
  @Override
  public WspmWaterBody getWater( )
  {
    final Feature parent = getFeature().getParent();
    if( parent != null && QNameUtilities.equals( parent.getFeatureType().getQName(), IWspmConstants.NS_WSPM, "WaterBody" ) ) //$NON-NLS-1$
      return new WspmWaterBody( parent );

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#setBigStation(java.math.BigDecimal)
   */
  @Override
  public void setBigStation( BigDecimal bigStation )
  {
    setProperty( ProfileFeatureFactory.QNAME_STATION, bigStation );
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#setSrsName(java.lang.String)
   */
  @Override
  public void setSrsName( String srsName )
  {
    getFeature().setProperty( QNAME_SRS, srsName );

  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#setStation(double)
   */
  @Override
  public void setStation( double station )
  {
    final BigDecimal bigStation = ProfilUtil.stationToBigDecimal( station );
    setBigStation( bigStation );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getFeature()
   */
  @Override
  public Feature getFeature( )
  {
    return this;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getGmlID()
   */
  @Override
  public String getGmlID( )
  {
    return this.getId();
  }

}
