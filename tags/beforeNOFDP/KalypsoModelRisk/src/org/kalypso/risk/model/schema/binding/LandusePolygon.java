package org.kalypso.risk.model.schema.binding;

import org.kalypso.risk.model.tools.functionParser.ParseFunction;
import org.kalypso.risk.plugin.KalypsoRiskDebug;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class LandusePolygon extends AbstractFeatureBinder implements ILandusePolygon
{
  private long m_statisticsNumberOfRasterCells = 0;

  private double m_statisticsAverageAnnualDamage = 0.0;

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

  public void setGeometry( GM_Surface< ? > surface )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_GEOMETRY, surface );
  }

  public void setStyleType( String styleType )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_SLDSTYLE, styleType );
  }

  public String getStyleType( )
  {
    final Object styleProp = getFeature().getProperty( ILandusePolygon.PROPERTY_SLDSTYLE );
    return (styleProp != null && styleProp != "") ? styleProp.toString() : "_DEFAULT_STYLE_";
  }

  public void setLanduseClass( final Feature landuseClassFeature )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_LANDUSE_CLASS, landuseClassFeature );
  }

  public int getLanduseClassOrdinalNumber( )
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
    return getFeature().getDefaultGeometryProperty().contains( position );
  }

  public double getDamageValue( double depth )
  {
    final ParseFunction damageFunction = getDamageFunction();
    final Double assetValue = getAssetValue();

    if( damageFunction == null || assetValue == null )
      return Double.NaN;

    try
    {
      // the returned calculated damage value must not be greater than the input asset value!
      // So, the value of the damage function must be less than 1, because there can be no greater damage than the
      // specified asset value!
      final double damagefunctionValue = damageFunction.getResult( depth ) / 100;

      if( damagefunctionValue > 1 )
        KalypsoRiskDebug.OPERATION.printf( "%s", "WARNING: damage value > asset value!\n" );

      return assetValue * damagefunctionValue;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return Double.NaN;
    }
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

    if( damageFunctionProp != null && getDamageFunctionProp() != "" )
    {
      final ParseFunction damageFunction = new ParseFunction( damageFunctionProp.toString() );

      // check if function is parsable
      if( !damageFunction.parse() )
        throw new IllegalArgumentException( "Damage function not parsable: " + getDamageFunctionProp().toString() );
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

}
