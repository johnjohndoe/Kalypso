package org.kalypso.risk.model.schema.binding;

import java.util.HashMap;
import java.util.Map;

import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.tools.functionParser.ParseFunction;
import org.kalypso.risk.model.utils.RiskPolygonStatistics;
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

  private final Map<Double, RiskPolygonStatistics> m_statistics = new HashMap<Double, RiskPolygonStatistics>();

  //
  // private double m_riskBorderLowMiddle = Double.NaN;
  //
  // private double m_riskBorderMiddleHigh = Double.NaN;

  public LandusePolygon( final Feature featureToBind )
  {
    super( featureToBind, QNAME );

    // try
    // {
    // if( m_isUrbanLanduseType != null )
    // if( m_isUrbanLanduseType )
    // {
    // m_riskBorderLowMiddle = 0.01 * m_assetValue * m_damageFunction.getResult( 2.0 ) / 100.0;
    // m_riskBorderMiddleHigh = 0.08 * m_assetValue * m_damageFunction.getResult( 1.0 ) / 100.0;
    // }
    // else
    // {
    // m_riskBorderLowMiddle = 0.01 * m_assetValue * m_damageFunction.getResult( 2.0 ) / 100.0;
    // m_riskBorderMiddleHigh = 0.03 * m_assetValue * m_damageFunction.getResult( 1.0 ) / 100.0;
    // }
    // }
    // catch( Exception e )
    // {
    // e.printStackTrace();
    // }
  }

  public void setGeometry( final GM_Surface< ? > surface )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_GEOMETRY, surface );
  }

  public void setStyleType( final String styleType )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_SLDSTYLE, styleType );
  }

  public String getStyleType( )
  {
    final Object styleProp = getFeature().getProperty( ILandusePolygon.PROPERTY_SLDSTYLE );
    return (styleProp != null && styleProp != "") ? styleProp.toString() : "_DEFAULT_STYLE_"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setLanduseClass( final Feature landuseClassFeature )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_LANDUSE_CLASS, landuseClassFeature );
  }

  public Integer getLanduseClassOrdinalNumber( )
  {
    return (Integer) getFeature().getProperty( ILandusePolygon.PROPERTY_ORDNUMBER );
  }

  public Boolean isUrbanLanduseType( )
  {
    final Object isUrbanTypeProperty = getFeature().getProperty( ILandusePolygon.PROPERTY_ISURBANTYPE );
    if( isUrbanTypeProperty instanceof Boolean )
      return (Boolean) isUrbanTypeProperty;
    else
      return null;
  }

  public boolean contains( final GM_Position position )
  {
    return getFeature().getDefaultGeometryPropertyValue().contains( position );
  }

  public double getDamageValue( final double depth )
  {
    final ParseFunction damageFunction = getDamageFunction();
    final Double assetValue = getAssetValue();

    if( damageFunction == null || assetValue == null )
      return Double.NaN;

    try
    {
      // the returned calculated damage value must not be greater than the input asset value!
      // So, the value of the damage function must be less than or equal '1', because there can be no greater damage
      // than the specified asset value!
      double damagefunctionValue = damageFunction.getResult( depth ) / 100;

      if( damagefunctionValue > 1 )
      {
        KalypsoRiskDebug.OPERATION.printf( "%s", Messages.getString( "org.kalypso.risk.model.schema.binding.LandusePolygon.3" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        damagefunctionValue = 1.0;
      }

      return assetValue * damagefunctionValue;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return Double.NaN;
    }
  }

  public void updateStatistics( final double value, final double returnPeriod )
  {
    RiskPolygonStatistics polygonStatistics = m_statistics.get( returnPeriod );
    if( polygonStatistics == null )
    {
      polygonStatistics = new RiskPolygonStatistics( returnPeriod );
      m_statistics.put( returnPeriod, polygonStatistics );
    }

    polygonStatistics.update( value );
  }

  /**
   * adds a average annual damage value to the polygon
   */
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

  public double getStatisticsAverageAnnualDamage( )
  {
    return m_statisticsAverageAnnualDamage;
  }

  private Object getDamageFunctionProp( )
  {
    return getFeature().getProperty( ILandusePolygon.PROPERTY_DAMAGE_FUNCTION );
  }

  private Object getAssetValueProp( )
  {
    return getFeature().getProperty( ILandusePolygon.PROPERTY_ASSET_VALUE );
  }

  private ParseFunction getDamageFunction( )
  {
    final Object damageFunctionProp = getDamageFunctionProp();

    if( damageFunctionProp != null && getDamageFunctionProp() != "" ) //$NON-NLS-1$
    {
      final ParseFunction damageFunction = new ParseFunction( damageFunctionProp.toString() );

      // check if function is parsable
      if( !damageFunction.parse() )
        throw new IllegalArgumentException( Messages.getString( "org.kalypso.risk.model.schema.binding.LandusePolygon.5" ) + getDamageFunctionProp().toString() ); //$NON-NLS-1$
      else
        return damageFunction;
    }
    else
      return null;
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
  public GM_Surface< ? > getGeometry( )
  {
    return getProperty( ILandusePolygon.PROPERTY_GEOMETRY, GM_Surface.class );
  }

  /**
   * @see org.kalypso.risk.model.schema.binding.ILandusePolygon#getLanduseClass()
   */
  public ILanduseClass getLanduseClass( final IRasterizationControlModel model )
  {
    final Object property = getFeature().getProperty( ILandusePolygon.PROPERTY_LANDUSE_CLASS );
    final Feature feature = FeatureHelper.resolveLinkedFeature( model.getFeature().getWorkspace(), property );

    return new LanduseClass( feature );
  }

}
