package org.kalypso.model.wspm.core.gml;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.model.feature.AbstractCachedFeature;

public class ProfileFeatureBinding extends AbstractCachedFeature implements IProfileFeature
{
  private GM_Curve m_curve = null;

  private IProfil m_iProfile = null;

  public ProfileFeatureBinding( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
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
    if( m_curve == null || isDirty( QNAME_LINE ) )
    {
      m_curve = (GM_Curve) getFeature().getProperty( QNAME_LINE );

      if( isDirty( QNAME_LINE ) )
      {
        setValid( QNAME_LINE );
      }
    }

    return m_curve;
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#getProfil()
   */
  @Override
  public IProfil getProfil( )
  {
    try
    {
      if( m_iProfile == null || isDirty( QNAME_OBS_MEMBERS, QNAME_STATION ) )
      {
        m_iProfile = toProfile();
        setValid( QNAME_OBS_MEMBERS, QNAME_STATION );
      }

      return m_iProfile;
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
    final Feature parent = getFeature().getOwner();
    if( parent != null && QNameUtilities.equals( parent.getFeatureType().getQName(), IWspmConstants.NS_WSPM, "WaterBody" ) )
    {
      return new WspmWaterBody( parent );
    }

    return null;
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#setBigStation(java.math.BigDecimal)
   */
  @Override
  public void setBigStation( final BigDecimal bigStation )
  {
    setProperty( ProfileFeatureFactory.QNAME_STATION, bigStation );
  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#setSrsName(java.lang.String)
   */
  @Override
  public void setSrsName( final String srsName )
  {
    getFeature().setProperty( QNAME_SRS, srsName );

  }

  /**
   * @see org.kalypso.model.wspm.core.gml.IProfileFeature#setStation(double)
   */
  @Override
  public void setStation( final double station )
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

  public String getProfileType( )
  {
    return getProperty( ProfileFeatureFactory.QNAME_TYPE, String.class );
  }

  public void setProfileType( final String type )
  {
    setProperty( ProfileFeatureFactory.QNAME_TYPE, type );
  }

  private IProfil toProfile( )
  {
    /* profile type */
    final String type = getProfileType();
    if( type == null )
    {
      return null;
    }

    /* observation of profile */
    final IObservation<TupleResult> observation = ObservationFeatureFactory.toObservation( this );

    final IProfil profil = ProfilFactory.createProfil( type, observation );

    /* station of profile */
    final BigDecimal bigStation = (BigDecimal) getProperty( ProfileFeatureFactory.QNAME_STATION );
    if( bigStation != null )
    {
      final double station = bigStation.doubleValue();
      profil.setStation( station );
    }

    /* Some metadata */
    final String crs = getSrsName();
    profil.setProperty( IWspmConstants.PROFIL_PROPERTY_CRS, crs );

    /* building of profile */
    // REMARK: handle buildings before table, because the setBuilding method resets the
    // corresponding table properties.
    final IObservation<TupleResult>[] profileObjects = getProfileObjects();
    if( profileObjects.length > 0 )
    {
      profil.createProfileObjects( profileObjects );
    }

    return profil;
  }

  @SuppressWarnings("unchecked")
  private IObservation<TupleResult>[] getProfileObjects( )
  {
    final List< ? > objects = (List< ? >) getProperty( QNAME_OBS_MEMBERS );
    if( objects.size() == 0 )
    {
      return new IObservation[] {};
    }

    final List<IObservation<TupleResult>> myResults = new ArrayList<IObservation<TupleResult>>();

    // iterate over all profile objects and create its IProfileObject representation
    for( final Object obj : objects )
    {
      final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( (Feature) obj );
      myResults.add( obs );
    }

    return myResults.toArray( new IObservation[] {} );
  }
}
