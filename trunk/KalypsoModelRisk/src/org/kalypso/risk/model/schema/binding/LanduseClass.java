package org.kalypso.risk.model.schema.binding;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public class LanduseClass extends Feature_Impl implements ILanduseClass
{
  public LanduseClass( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  final List<IRiskLanduseStatistic> m_statList = new ArrayList<IRiskLanduseStatistic>();

  private double m_statisticsAverageAnnualDamage;

  private int m_statisticsNumberOfRasterCells;

  private double m_cellSize = Double.NaN;



  @Override
  public void setName( final String name )
  {
    setProperty( ILanduseClass.PROP_NAME, name );
  }

  @Override
  public void setDescription( final String desc )
  {
    setProperty( ILanduseClass.PROP_DESCRIPTION, desc );
  }

  @Override
  public String getName( )
  {
    return (String) getProperty( ILanduseClass.PROP_NAME );
  }

  @Override
  public String getDescription( )
  {
    return (String) getProperty( ILanduseClass.PROP_DESCRIPTION );
  }

  @Override
  public RGB getColorStyle( )
  {
    return (RGB) getProperty( ILanduseClass.PROP_COLOR_STYLE );
  }

  @Override
  public void setColorStyle( final RGB rgb )
  {
    setProperty( ILanduseClass.PROP_COLOR_STYLE, rgb );
  }

  @Override
  public void setOrdinalNumber( final int value )
  {
    setProperty( ILanduseClass.PROP_ORDINAL_NUMBER, value );
  }

  @Override
  public int getOrdinalNumber( )
  {
    final Integer value = (Integer) getProperty( ILanduseClass.PROP_ORDINAL_NUMBER );
    return value == null ? 0 : value.intValue();
  }

  @Override
  public double getMaxAnnualDamage( )
  {
    final Double value = (Double) getProperty( ILanduseClass.PROP_MAX_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  @Override
  public double getMinAnnualDamage( )
  {
    final Double value = (Double) getProperty( ILanduseClass.PROP_MIN_DAMAGE );
    return value == null ? Double.MAX_VALUE : value.intValue();
  }

  @Override
  public double getTotalDamage( )
  {
    final Double value = (Double) getProperty( ILanduseClass.PROP_TOTAL_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  @Override
  public double getAverageAnnualDamage( )
  {
    final Double value = (Double) getProperty( ILanduseClass.PROP_ANNUAL_AVERAGE_DAMAGE );
    return value == null ? 0 : value.doubleValue();
  }

  @Override
  public void setMaxAnnualDamage( final double value )
  {
    setProperty( ILanduseClass.PROP_MAX_DAMAGE, value );
  }

  @Override
  public void setMinAnnualDamage( final double value )
  {
    setProperty( ILanduseClass.PROP_MIN_DAMAGE, value );
  }

  @Override
  public void setTotalDamage( final double value )
  {
    setProperty( ILanduseClass.PROP_TOTAL_DAMAGE, value );
  }

  @Override
  public void setAverageAnnualDamage( final double value )
  {
    setProperty( ILanduseClass.PROP_ANNUAL_AVERAGE_DAMAGE, value );
  }

  @Override
  public String getDamageFunctionGmlID( )
  {
    final Object property = getProperty( ILanduseClass.PROP_DAMAGE_FUNCTION_LINK );
    if( property != null && property instanceof IXLinkedFeature )
      return ((IXLinkedFeature) property).getFeatureId();
    return ""; //$NON-NLS-1$
  }

  @Override
  public void setDamageFunction( final IDamageFunction damageFunction )
  {
    final String xFeaturePath = IRasterizationControlModel.MODEL_FILENAME_GML + "#" + damageFunction.getId(); //$NON-NLS-1$
    setLink( ILanduseClass.PROP_DAMAGE_FUNCTION_LINK, xFeaturePath, damageFunction.getFeatureType() );
  }

  @Override
  public IAssetValueClass getAssetValue( )
  {
    final Object property = getProperty( ILanduseClass.PROP_ASSET_VALUE_LINK );
    final Feature assetFeature = FeatureHelper.resolveLinkedFeature( getWorkspace(), property );
    if( assetFeature == null )
      return null;

    final IAssetValueClass assetValueClass = (IAssetValueClass) assetFeature.getAdapter( IAssetValueClass.class );
    return assetValueClass;
  }

  @Override
  public void setAssetValue( final IAssetValueClass assetValueClass )
  {
    final String xFeaturePath = IRasterizationControlModel.MODEL_FILENAME_GML + "#" + assetValueClass.getId(); //$NON-NLS-1$
    setLink( ILanduseClass.PROP_ASSET_VALUE_LINK, xFeaturePath, assetValueClass.getFeatureType() );
  }

  @Override
  public void updateStatistic( final int returnPeriod )
  {
    for( final IRiskLanduseStatistic entry : m_statList )
    {
      if( entry.getReturnPeriod() == returnPeriod )
        entry.finish();
    }
  }

  public IRiskLanduseStatistic getStatisticEntry( final int returnPeriod )
  {
    final List<IRiskLanduseStatistic> statisticList = getLanduseStatisticList();

    for( final IRiskLanduseStatistic entry : statisticList )
    {
      if( entry.getReturnPeriod() == returnPeriod )
        return entry;
    }
    return null;
  }

  @Override
  public List<IRiskLanduseStatistic> getLanduseStatisticList( )
  {
    final List<IRiskLanduseStatistic> statList = new ArrayList<IRiskLanduseStatistic>();

    final FeatureList list = (FeatureList) getProperty( ILanduseClass.PROP_DAMAGE_STATISTIC_LIST );
    for( final Object object : list )
    {
      if( object instanceof Feature )
      {
        final Feature feature = (Feature) object;
        final IRiskLanduseStatistic statEntry = (IRiskLanduseStatistic) feature.getAdapter( IRiskLanduseStatistic.class );
        statList.add( statEntry );
      }
    }
    return statList;
  }

  @Override
  public boolean containsStatisticEntry( final int returnPeriod )
  {
    for( final IRiskLanduseStatistic entry : m_statList )
      if( entry.getReturnPeriod() == returnPeriod )
        return true;
    return false;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#createNewLanduseClass()
   */
  @Override
  public IRiskLanduseStatistic createNewStatisticEntry( )
  {
    final FeatureList list = (FeatureList) getProperty( ILanduseClass.PROP_DAMAGE_STATISTIC_LIST );

    try
    {
      final Feature feature = FeatureHelper.createFeatureForListProp( list, IRiskLanduseStatistic.QNAME, -1 );
      final IRiskLanduseStatistic listEntry = (IRiskLanduseStatistic) feature.getAdapter( IRiskLanduseStatistic.class );
      m_statList.add( listEntry );
      return listEntry;
    }
    catch( final GMLSchemaException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#getStatistic(int, double)
   */
  @Override
  public IRiskLanduseStatistic getStatistic( final int returnPeriod )
  {
    for( final IRiskLanduseStatistic entry : m_statList )
      if( entry.getReturnPeriod() == returnPeriod )
        return entry;
    return null;

  }

  /**
   * adds a average annual damage value to the polygon
   */
  @Override
  public void updateStatisticsAverageAnnualDamage( final double value )
  {
    /* check for min / max */
    if( value < getMinAnnualDamage() )
      setMinAnnualDamage( value );
    if( value > getMaxAnnualDamage() )
      setMaxAnnualDamage( value );

    /* update total damage value of the current landuse class */
    final double totalDamage = getTotalDamage() + value * m_cellSize;
    setTotalDamage( totalDamage );

    /* get the current overall average annual damage value (€/a) */
    final double currentValue = m_statisticsAverageAnnualDamage * m_statisticsNumberOfRasterCells;

    /* add the new value */
    final double updatedValue = currentValue + value;

    /* raise number of cells */
    m_statisticsNumberOfRasterCells++;

    /* calculate the average annual damage value (€/a) per cell */
    m_statisticsAverageAnnualDamage = updatedValue / m_statisticsNumberOfRasterCells;

    setAverageAnnualDamage( m_statisticsAverageAnnualDamage );
  }

  public double getStatisticsAverageAnnualDamage( )
  {
    return m_statisticsAverageAnnualDamage;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#setCellSize(int)
   */
  @Override
  public void setCellSize( final double cellSize )
  {
    m_cellSize = cellSize;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#getCellSize()
   */
  @Override
  public double getCellSize( )
  {
    return m_cellSize;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILanduseClass#clearStatisticEntries()
   */
  @Override
  public void clearStatisticEntries( )
  {
    m_statList.clear();
    final FeatureList list = (FeatureList) getProperty( ILanduseClass.PROP_DAMAGE_STATISTIC_LIST );
    list.clear();
  }
}
