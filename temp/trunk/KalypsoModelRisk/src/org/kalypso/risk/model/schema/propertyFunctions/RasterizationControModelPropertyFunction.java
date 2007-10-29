package org.kalypso.risk.model.schema.propertyFunctions;

import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.virtual.VirtualFunctionValuePropertyType;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

public class RasterizationControModelPropertyFunction extends FeaturePropertyFunction
{
  private final static QName QNAME = new QName( NS.GML3, "name" );

  private final static QName VPROP_DAMAGE_FUNCTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionName" );

  private final static QName XLINK_DAMAGE_FUNCTION = new QName( KalypsoRiskSchemaCatalog.NS_RASTERIZATION_CONTROL_MODEL, "damageFunctionLink" );

  @Override
  public void init( Map<String, String> properties )
  {
    // TODO Auto-generated method stub

  }

  public Object getValue( Feature feature, IPropertyType pt, Object currentValue )
  {
    final QName ptQName = pt.getQName();

    final IPropertyType property = feature.getFeatureType().getProperty( ptQName );
    if( property != null && !(property instanceof VirtualFunctionValuePropertyType) )
      return getFirstValue( feature.getProperty( ptQName ) );

    Feature member = null;

    if( ptQName.equals( VPROP_DAMAGE_FUNCTION ) )
    {
      member = (Feature) feature.getProperty( XLINK_DAMAGE_FUNCTION );
      if( member == null )
        return "";
      else
        return getFirstValue( member.getProperty( QNAME ) );
    }
//    else if( ptQName.equals( VPROP_... ) )
//    {
//      member = (Feature) feature.getProperty( XLINK_... );
//      if( member == null )
//        return "";
//      else
//        return getValue( member.getProperty( QNAME ) );
//    }

    if( member != null && member.getFeatureType().getProperty( ptQName ) != null )
      return getFirstValue( member.getProperty( ptQName ) );

//    member = (Feature) feature.getProperty( m_vegetationClsMember );
//    if( member != null && member.getFeatureType().getProperty( ptQName ) != null )
//      return getFirstValue( member.getProperty( ptQName ) );
//
//    member = (Feature) feature.getProperty( m_damageFunction );
//    if( member != null && member.getFeatureType().getProperty( ptQName ) != null )
//      return getFirstValue( member.getProperty( ptQName ) );
//
//    member = (Feature) feature.getProperty( m_eddyViscosityClsMember );
//    if( member != null && member.getFeatureType().getProperty( ptQName ) != null )
//      return getFirstValue( member.getProperty( ptQName ) );

    return null;
  }

  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @SuppressWarnings("unchecked")
  private Object getFirstValue( final Object object )
  {
    if( object == null )
      return "";
    if( object instanceof List )
      return ((List<Object>) object).get( 0 );
    else
      return object;
  }

}
