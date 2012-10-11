package org.kalypso.risk.model.schema.binding;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

public class LanduseClass extends Feature_Impl implements ILanduseClass
{
  public LanduseClass( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

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
    return (String)getProperty( ILanduseClass.PROP_NAME );
  }

  @Override
  public String getDescription( )
  {
    return (String)getProperty( ILanduseClass.PROP_DESCRIPTION );
  }

  @Override
  public RGB getColorStyle( )
  {
    return (RGB)getProperty( ILanduseClass.PROP_COLOR_STYLE );
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
    final Integer value = (Integer)getProperty( ILanduseClass.PROP_ORDINAL_NUMBER );
    return value == null ? 0 : value.intValue();
  }

  @Override
  public String getDamageFunctionGmlID( )
  {
    final Object property = getProperty( ILanduseClass.PROP_DAMAGE_FUNCTION_LINK );
    if( property != null && property instanceof IXLinkedFeature )
      return ((IXLinkedFeature)property).getFeatureId();
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

    final IAssetValueClass assetValueClass = (IAssetValueClass)assetFeature.getAdapter( IAssetValueClass.class );
    return assetValueClass;
  }

  @Override
  public void setAssetValue( final IAssetValueClass assetValueClass )
  {
    final String xFeaturePath = IRasterizationControlModel.MODEL_FILENAME_GML + "#" + assetValueClass.getId(); //$NON-NLS-1$
    setLink( ILanduseClass.PROP_ASSET_VALUE_LINK, xFeaturePath, assetValueClass.getFeatureType() );
  }
}