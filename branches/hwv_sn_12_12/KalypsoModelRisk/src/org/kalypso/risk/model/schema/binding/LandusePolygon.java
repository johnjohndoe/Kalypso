package org.kalypso.risk.model.schema.binding;

import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.utils.FunctionParserCache;
import org.kalypso.risk.plugin.KalypsoRiskDebug;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class LandusePolygon extends AbstractFeatureBinder implements ILandusePolygon
{
  private long m_statisticsNumberOfRasterCells = 0;

  private double m_statisticsAverageAnnualDamage = 0.0;

  public LandusePolygon( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  @Override
  public void setGeometry( final GM_Surface< ? > surface )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_GEOMETRY, surface );
  }

  @Override
  public void setStyleType( final String styleType )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_SLDSTYLE, styleType );
  }

  @Override
  public String getStyleType( )
  {
    final Object styleProp = getFeature().getProperty( ILandusePolygon.PROPERTY_SLDSTYLE );
    return (styleProp != null && styleProp != "") ? styleProp.toString() : "_DEFAULT_STYLE_"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void setLanduseClass( final Feature landuseClassFeature )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_LANDUSE_CLASS, landuseClassFeature );
  }

  @Override
  public Integer getLanduseClassOrdinalNumber( )
  {
    return (Integer) getFeature().getProperty( ILandusePolygon.PROPERTY_ORDNUMBER );
  }

  @Override
  public Boolean isUrbanLanduseType( )
  {
    final Object isUrbanTypeProperty = getFeature().getProperty( ILandusePolygon.PROPERTY_ISURBANTYPE );
    if( isUrbanTypeProperty instanceof Boolean )
      return (Boolean) isUrbanTypeProperty;
    else
      return null;
  }

  @Override
  public boolean contains( final GM_Position position )
  {
    return getFeature().getDefaultGeometryPropertyValue().contains( position );
  }

  @Override
  public double getDamageValue( final double depth )
  {
    final String damageFunctionProp = getDamageFunctionProp();
    final Double assetValue = getAssetValue();

    if( assetValue == null || damageFunctionProp == null || getDamageFunctionProp().length() == 0 )
      return Double.NaN;

    try
    {
      // Using this cache here, as parsing the function is quite slow; better maybe: cache function in this object.
      // But this can only be done, if we use the new feature binding stuff instead of adapting to feature.
      final double damagePercentage = FunctionParserCache.getValue( damageFunctionProp, depth );

      // the returned calculated damage value must not be greater than the input asset value!
      // So, the value of the damage function must be less than or equal '1', because there can be no greater damage
      // than the specified asset value!
      if( damagePercentage < 0.0 )
        return Double.NaN;

      if( damagePercentage > 100.0 )
      {
        KalypsoRiskDebug.OPERATION.printf( "%s", Messages.getString( "org.kalypso.risk.model.schema.binding.LandusePolygon.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        return assetValue;
      }

      return assetValue * damagePercentage / 100.0;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return Double.NaN;
    }
  }

  /**
   * adds a average annual damage value to the polygon
   */
  @Override
  public void updateStatisticsAverageAnnualDamage( final double value )
  {
    /* get the current overall average annual damage value (€/a) */
    final double currentValue = m_statisticsAverageAnnualDamage * m_statisticsNumberOfRasterCells;

    /* add the new value */
    final double updatedValue = currentValue + value;

    /* raise number of cells */
    m_statisticsNumberOfRasterCells++;

    /* calculate the average annual damage value (€/a) per cell */
    m_statisticsAverageAnnualDamage = updatedValue / m_statisticsNumberOfRasterCells;
  }

  @Override
  public double getStatisticsAverageAnnualDamage( )
  {
    return m_statisticsAverageAnnualDamage;
  }

  private String getDamageFunctionProp( )
  {
    return (String) getFeature().getProperty( ILandusePolygon.PROPERTY_DAMAGE_FUNCTION );
  }

  private Object getAssetValueProp( )
  {
    return getFeature().getProperty( ILandusePolygon.PROPERTY_ASSET_VALUE );
  }

  private Double getAssetValue( )
  {
    final Object assetValueProp = getAssetValueProp();

    if( assetValueProp != null )
      return ((Double) assetValueProp).doubleValue();
    else
      return null;
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILandusePolygon#getGeometry()
   */
  @Override
  public GM_Surface< ? > getGeometry( )
  {
    return getProperty( ILandusePolygon.PROPERTY_GEOMETRY, GM_Surface.class );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILandusePolygon#getLanduseClass()
   */
  @Override
  public ILanduseClass getLanduseClass( final IRasterizationControlModel model )
  {
    final Object property = getFeature().getProperty( ILandusePolygon.PROPERTY_LANDUSE_CLASS );
    // TODO: use the workspace of the bound feature, no need for the model to be given as parameter...
    final Feature feature = FeatureHelper.resolveLinkedFeature( model.getFeature().getWorkspace(), property );

    return new LanduseClass( feature );
  }

}
