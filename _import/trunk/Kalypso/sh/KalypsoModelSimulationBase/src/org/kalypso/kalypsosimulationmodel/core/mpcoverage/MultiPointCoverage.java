package org.kalypso.kalypsosimulationmodel.core.mpcoverage;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.schema.GmlImitationsConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * 
 * @author Patrice Congo
 */
public class MultiPointCoverage<RangSetCls extends IFeatureWrapper2> implements IMultipointCoverage<RangSetCls>
{

  private final Feature mpFeature;

  private final FeatureRangeSet<RangSetCls> rangeSet;

  private final MultiPoint domainSet;

  public MultiPointCoverage( Feature mpFeature, Class<RangSetCls> rangeSetClass ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( mpFeature, Messages.getString( "MultiPointCoverage.0" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNull( rangeSetClass, Messages.getString( "MultiPointCoverage.1" ) ); //$NON-NLS-1$

    if( !Util.directInstanceOf( mpFeature, GmlImitationsConsts.WBGML_F_MULTIPOINT_COVERAGE ) )
    {
      throw new IllegalArgumentException( Messages.getString( "MultiPointCoverage.2" ) + //$NON-NLS-1$
          GmlImitationsConsts.WBGML_F_MULTIPOINT_COVERAGE + Messages.getString( "MultiPointCoverage.3" ) + mpFeature.getFeatureType().getQName() ); //$NON-NLS-1$
    }

    // this.rangeSetClass=rangeSetClass;
    this.mpFeature = mpFeature;
    MultiPoint mp = new MultiPoint( (Feature) mpFeature.getProperty( GmlImitationsConsts.WBGML_PROP_MULTIPOINT_DOMAIN )
    // mpFeature,
    // GmlImitationsConsts.WBGML_PROP_MULTIPOINT_DOMAIN
    );
    FeatureRangeSet<RangSetCls> fl = new FeatureRangeSet<RangSetCls>( (Feature) mpFeature.getProperty( GmlImitationsConsts.WBGML_PROP_RANGESET ), rangeSetClass );
    if( mp.size() != fl.size() )
    {
      throw new IllegalArgumentException( "Size mismacth DomainSet.size=" + mp.size() + //$NON-NLS-1$
          " RangeSetSize.size=" + fl.size() ); //$NON-NLS-1$
    }
    this.domainSet = mp;
    this.rangeSet = fl;
  }

  public void addCoverageEntry( RangSetCls rangeValue, GM_Point position ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( rangeValue, Messages.getString( "MultiPointCoverage.6" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNull( position, Messages.getString( "MultiPointCoverage.7" ) ); //$NON-NLS-1$
    rangeSet.add( rangeValue );
    domainSet.add( position );
  }

  public List<GM_Point> getApplicablePosition( RangSetCls rangeValue ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( rangeValue, Messages.getString( "MultiPointCoverage.8" ) ); //$NON-NLS-1$
    final int SIZE = rangeSet.size();
    List<GM_Point> pointList = new ArrayList<GM_Point>( SIZE );
    for( int i = 0; i < SIZE; i++ )
    {
      if( rangeValue.equals( rangeSet.get( i ) ) )
      {
        pointList.add( domainSet.get( i ) );
      }
    }
    return pointList;
  }

  public MultiPoint getDomain( )
  {
    return domainSet;
  }

  public RangSetCls getRangeValue( GM_Point location ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( location, Messages.getString( "MultiPointCoverage.9" ) ); //$NON-NLS-1$
    int index = domainSet.indexOf( location );
    if( index >= 0 )
    {
      return rangeSet.get( index );
    }
    else
    {
      return null;
    }
  }

  public List<RangSetCls> getRangeValues( GM_Polygon region ) throws IllegalArgumentException
  {
    final int SIZE = domainSet.size();
    List<RangSetCls> rs = new ArrayList<RangSetCls>( SIZE );
    for( int i = 0; i < SIZE; i++ )
    {
      if( region.contains( domainSet.get( i ) ) )
      {
        rs.add( rangeSet.get( i ) );
      }
    }
    return rs;
  }

  public void removeCoveredPosition( GM_Point position ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( position, Messages.getString( "MultiPointCoverage.10" ) ); //$NON-NLS-1$
    for( int index = domainSet.size() - 1; index >= 0; index-- )
    {
      if( position.equals( domainSet.get( index ) ) )
      {
        rangeSet.remove( index );
        domainSet.remove( index );
      }
    }
  }

  public void removeRangeValue( RangSetCls rangeValue ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( rangeValue, Messages.getString( "MultiPointCoverage.11" ) ); //$NON-NLS-1$
    for( int index = rangeSet.size() - 1; index >= 0; index-- )
    {
      if( rangeValue.equals( rangeSet.get( index ) ) )
      {
        rangeSet.remove( index );
        domainSet.remove( index );
      }
    }
  }

  public Feature getWrappedFeature( )
  {
    return mpFeature;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
   */
  public String getGmlID( )
  {
    return mpFeature.getId();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  public String getDescription( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( String desc )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( String name )
  {
    // TODO Auto-generated method stub

  }

}
