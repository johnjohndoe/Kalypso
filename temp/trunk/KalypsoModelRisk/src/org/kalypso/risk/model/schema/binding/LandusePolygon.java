package org.kalypso.risk.model.schema.binding;

import org.kalypso.risk.model.tools.functionParser.ParseFunction;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class LandusePolygon extends AbstractFeatureBinder implements ILandusePolygon
{
  private long m_statisticsNumberOfRasterCells = 0;

  private double m_statisticsAverageAnnualDamage = 0.0;

  private String m_styleType = null;

  private Integer m_landuseClassOrdinalNumber = null;

  private ParseFunction m_damageFunction = null;

  private Double m_assetValue = null;

  private final Boolean m_isUrbanLanduseType;

  //
  // private double m_riskBorderLowMiddle = Double.NaN;
  //
  // private double m_riskBorderMiddleHigh = Double.NaN;

  public LandusePolygon( final Feature featureToBind )
  {
    super( featureToBind, QNAME );

    final Object styleProp = getFeature().getProperty( ILandusePolygon.PROPERTY_SLDSTYLE );
    m_styleType = (styleProp != null && styleProp != "") ? styleProp.toString() : "_DEFAULT_STYLE_";

    final Object isUrbanTypeProperty = getFeature().getProperty( ILandusePolygon.PROPERTY_ISURBANTYPE );
    if( isUrbanTypeProperty instanceof Boolean )
      m_isUrbanLanduseType = (Boolean) isUrbanTypeProperty;
    else
      m_isUrbanLanduseType = null;
    m_landuseClassOrdinalNumber = (Integer) getFeature().getProperty( ILandusePolygon.PROPERTY_ORDNUMBER );

    final Object damageFunctionProp = getFeature().getProperty( ILandusePolygon.PROPERTY_DAMAGE_FUNCTION );
    if( damageFunctionProp != null && damageFunctionProp != "" )
    {
      final Object assetValueProp = getFeature().getProperty( ILandusePolygon.PROPERTY_ASSET_VALUE );
      if( assetValueProp != null )
      {
        m_damageFunction = new ParseFunction( damageFunctionProp.toString() );
        if( !m_damageFunction.parse() )
          throw new IllegalArgumentException( "Damage function not parsable: " + damageFunctionProp.toString() );
        m_assetValue = ((Double) assetValueProp).doubleValue();
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
    }
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
    return m_styleType;
  }

  public void setLanduseClass( final Feature landuseClassFeature )
  {
    getFeature().setProperty( ILandusePolygon.PROPERTY_LANDUSE_CLASS, landuseClassFeature );
  }

  public int getLanduseClassOrdinalNumber( )
  {
    return m_landuseClassOrdinalNumber;
  }

  public Boolean isUrbanLanduseType( )
  {
    return m_isUrbanLanduseType;
  }

  public boolean contains( final GM_Position position )
  {
    return getFeature().getDefaultGeometryProperty().contains( position );
  }

  public double getDamageValue( double waterLevel )
  {
    if( m_damageFunction == null || m_assetValue == null )
      return Double.NaN;
    try
    {
      return m_assetValue * m_damageFunction.getResult( waterLevel ) / 100.0;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return Double.NaN;
    }
  }

  public double getRiskZone( double damageValue )
  {
    return 0.0;
    // if( m_isUrbanLanduseType == null || Double.isNaN( damageValue ) )
    // return Double.NaN;
    // if( m_isUrbanLanduseType )
    // {
    // if( damageValue < m_riskBorderLowMiddle )
    // return IRasterizationControlModel.RISKZONE_URBANAREA_LOW;
    // if( damageValue < m_riskBorderMiddleHigh )
    // return IRasterizationControlModel.RISKZONE_URBANAREA_MIDDLE;
    // return IRasterizationControlModel.RISKZONE_URBANAREA_HIGH;
    // }
    // else
    // {
    // if( damageValue < m_riskBorderLowMiddle )
    // return IRasterizationControlModel.RISKZONE_NONURBANAREA_LOW;
    // if( damageValue < m_riskBorderMiddleHigh )
    // return IRasterizationControlModel.RISKZONE_NONURBANAREA_MIDDLE;
    // return IRasterizationControlModel.RISKZONE_NONURBANAREA_HIGH;
    // }
  }

  public void updateStatisticsAverageAnnualDamage( final double value )
  {
    m_statisticsAverageAnnualDamage = m_statisticsAverageAnnualDamage * m_statisticsNumberOfRasterCells + value;
    m_statisticsNumberOfRasterCells++;
    m_statisticsAverageAnnualDamage = m_statisticsAverageAnnualDamage / m_statisticsNumberOfRasterCells;
  }

  public double getStatisticsAverageAnnualDamage( )
  {
    return m_statisticsAverageAnnualDamage;
  }

}
