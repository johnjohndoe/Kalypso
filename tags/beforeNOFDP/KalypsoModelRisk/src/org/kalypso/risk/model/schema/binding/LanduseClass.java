package org.kalypso.risk.model.schema.binding;

import java.util.List;

import org.eclipse.swt.graphics.RGB;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

public class LanduseClass extends AbstractFeatureBinder implements ILanduseClass
{
  public LanduseClass( final Feature featureToBind )
  {
    super( featureToBind, QNAME );
  }

  public RGB getColorStyle( )
  {
    return (RGB) getFeature().getProperty( ILanduseClass.PROP_COLOR_STYLE );
  }

  public void setColorStyle( final RGB rgb )
  {
    getFeature().setProperty( ILanduseClass.PROP_COLOR_STYLE, rgb );
  }

  public void setOrdinalNumber( int value )
  {
    getFeature().setProperty( ILanduseClass.PROP_ORDINAL_NUMBER, value );
  }

  public int getOrdinalNumber( )
  {
    final Integer value = (Integer) getFeature().getProperty( ILanduseClass.PROP_ORDINAL_NUMBER );
    return value == null ? 0 : value.intValue();
  }

  public double getMaxDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_MAX_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  public double getMinDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_MIN_DAMAGE );
    return value == null ? Double.MAX_VALUE : value.intValue();
  }

  public double getTotalDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_TOTAL_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  public double getAverageAnnualDamage( )
  {
    final Double value = (Double) getFeature().getProperty( ILanduseClass.PROP_ANNUAL_AVERAGE_DAMAGE );
    return value == null ? 0 : value.intValue();
  }

  public void setMaxDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_MAX_DAMAGE, value );
  }

  public void setMinDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_MIN_DAMAGE, value );
  }

  public void setTotalDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_TOTAL_DAMAGE, value );
  }

  public void setAverageAnnualDamage( double value )
  {
    getFeature().setProperty( ILanduseClass.PROP_ANNUAL_AVERAGE_DAMAGE, value );
  }

  public String getDamageFunctionGmlID( )
  {
    final Object property = getFeature().getProperty( ILanduseClass.PROP_DAMAGE_FUNCTION_LINK );
    if( property != null && property instanceof XLinkedFeature_Impl )
      return ((XLinkedFeature_Impl) property).getFeatureId();
    return "";
  }

  public void setDamageFunction( final IDamageFunction damageFunction )
  {
    final String xFeaturePath = IRasterizationControlModel.MODEL_NAME + "#" + damageFunction.getGmlID();
    final XLinkedFeature_Impl xFeature = new XLinkedFeature_Impl( getFeature(), damageFunction.getFeature().getParentRelation(), damageFunction.getFeature().getFeatureType(), xFeaturePath, "", "", "", "", "" );
    getFeature().setProperty( ILanduseClass.PROP_DAMAGE_FUNCTION_LINK, xFeature );
  }

  public IAssetValueClass getAssetValue( )
  {
    Feature parent = getFeature().getParent();
    if( parent == null )
      return null;

    final IRasterizationControlModel model = (IRasterizationControlModel) parent.getAdapter( IRasterizationControlModel.class );
    if( model == null )
      return null;

    final List<IAssetValueClass> assetValueClassesList = model.getAssetValueClassesList();

    // TODO: probably the data model is not correctly modelled; this reverse search is not nice at all
    // Possible solutions: 1) link from landuseClass to assesValue instead
    // or 2) link from landusePolygon to assetValue

    // // TODO For the moment, administration units are ignored; consider using administration units
    //
    final String landuseClassGmlID = getGmlID();
    for( final IAssetValueClass assetValueClass : assetValueClassesList )
    {
      if( assetValueClass.getLanduseClassGmlID().equals( landuseClassGmlID ) )
        return assetValueClass;
    }

    return null;
  }
}
