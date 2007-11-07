package org.kalypso.risk.model.schema.binding;

import org.kalypso.risk.model.tools.functionParser.ParseFunction;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

public class LandusePolygon extends AbstractFeatureBinder implements ILandusePolygon
{
  private String m_styleType = null;

  private Integer m_landuseClassOrdinalNumber = null;

  private ParseFunction m_damageFunction = null;

  private Double m_assetValue = null;

  public LandusePolygon( final Feature featureToBind )
  {
    super( featureToBind, QNAME );

    final Object styleProp = getFeature().getProperty( ILandusePolygon.PROPERTY_SLDSTYLE );
    m_styleType = (styleProp != null && styleProp != "") ? styleProp.toString() : "_DEFAULT_STYLE_";

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
      }
    }

    m_landuseClassOrdinalNumber = (Integer) getFeature().getProperty( ILandusePolygon.PROPERTY_ORDNUMBER );
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
      return m_assetValue * m_damageFunction.getResult( waterLevel );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return Double.NaN;
    }
  }

}
