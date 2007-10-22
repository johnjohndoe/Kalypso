package org.kalypso.kalypsosimulationmodel.core.roughness;

import javax.xml.namespace.QName;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Default implementation for {@link IRoughnessCls} which serves as a wrapper arround a wbd:Roughness feature
 * 
 * @author Patrice Congo
 */
public class RoughnessCls extends AbstractFeatureBinder implements IRoughnessCls
{
  /**
   * Create a Roughness object wrapping the given feature.
   * 
   * @param feature -
   *            the wbr:Roughness feature to wrapped
   * @throw IllegalArgumentException if feature is null or is note a wbr:Roughness feature
   */
  public RoughnessCls( Feature featureToBind ) throws IllegalArgumentException
  {
    super( featureToBind, KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
  }

  /**
   * This constructor creates {@link RoughnessCls} based on a wbr:RoughnessCls feature which is created as child of the
   * given parent feaure and linked to it by the property of the type specified by the argument propQName.
   * 
   * @param parentFeature
   *            the parent feature for the new wbr:Roughness class
   * @param propQName
   *            the Q-name of the linking property type
   * @throws IllegalArgumentException
   *             if workspace is null or the roughness collection is not part of the workspace
   */
  public RoughnessCls( Feature parentFeature, QName propQName ) throws IllegalArgumentException
  {
    super( Util.createFeatureAsProperty( parentFeature, propQName, KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS ), KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getAxAy()
   */
  public double getAxAy( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( feature, KalypsoModelRoughnessConsts.WBR_PROP_AXAY, Double.NaN );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getDp()
   */
  public double getDp( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( feature, KalypsoModelRoughnessConsts.WBR_PROP_DP, Double.NaN );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getEddy()
   */
  public double getEddyXX( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( feature, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XX, 500.0 );
  }

  public double getEddyYX( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( feature, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YX, 500.0 );
  }

  public double getEddyXY( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( feature, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XY, 500.0 );
  }

  public double getEddyYY( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( feature, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YY, 500.0 );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getKs()
   */
  public double getKs( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( feature, KalypsoModelRoughnessConsts.WBR_PROP_KS, Double.NaN );
  }

  public double getMarsh( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( feature, KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV, Double.NaN );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setAxAy(double)
   */
  public void setAxAy( double axay ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY, Double.valueOf( axay ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setDp(double)
   */
  public void setDp( double dp ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP, Double.valueOf( dp ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setEddy(double)
   */
  public void setEddyXX( double eddy ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XX, Double.valueOf( eddy ) );
  }

  public void setEddyYX( double eddy ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YX, Double.valueOf( eddy ) );
  }

  public void setEddyXY( double eddy ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XY, Double.valueOf( eddy ) );
  }

  public void setEddyYY( double eddy ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YY, Double.valueOf( eddy ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setKs(double)
   */
  public void setKs( double ks ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS, Double.valueOf( ks ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setMarsh(double)
   */
  public void setMarsh( double marsh ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV, Double.valueOf( marsh ) );
  }

  // /Ask nico for details
  // one is sure negatives are not allowed
  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#validate()
   */
  public RoughnessConfigConsistency validate( )
  {
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#configure(java.lang.String, double, double,
   *      double, double)
   */
  public RoughnessConfigConsistency configure( String name, double ks, double axay, double dp, double eddy_xx, double eddy_yx, double eddy_xy, double eddy_yy, double marsh )
  {
    RoughnessConfigConsistency check = validate( name, ks, axay, dp, eddy_xx, eddy_yx, eddy_xy, eddy_yy, marsh );
    if( check != RoughnessConfigConsistency.OK )
    {
      return check;
    }
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.GML_PROP_NAME, name );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS, Double.valueOf( ks ) );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP, Double.valueOf( dp ) );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XX, Double.valueOf( eddy_xx ) );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YX, Double.valueOf( eddy_yx ) );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XY, Double.valueOf( eddy_xy ) );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YY, Double.valueOf( eddy_yy ) );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY, Double.valueOf( axay ) );

    return RoughnessConfigConsistency.OK;
  }

  public static RoughnessConfigConsistency validate( String name, double ks, double axay, double dp, double eddy_xx, double eddy_yx, double eddy_xy, double eddy_yy, double marsh )
  {

    if( Assert.isNullOrEmpty( name ) )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_NAME;
    }

    if( ks < 0 )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_KS;
    }

    if( eddy_xx <= 0 )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_EDDY_XX;
    }
    if( eddy_yx <= 0 )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_EDDY_YX;
    }
    if( eddy_xy <= 0 )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_EDDY_XY;
    }
    if( eddy_yy <= 0 )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_EDDY_YY;
    }
    if( dp < 0 )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_DP;
    }
    if( axay < 0 )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_AXAY;
    }
    if( marsh < 0 )
    {
      return RoughnessConfigConsistency.ILLEGAL_VALUE_MARSH;
    }
    return RoughnessConfigConsistency.OK;
  }

  @Override
  public String toString( )
  {
    StringBuffer buf = new StringBuffer( 64 );
    buf.append( "RoughnessCls" ); //$NON-NLS-1$
    String id = getGmlID();
    if( id != null )
      buf.append( '{' ).append( id ).append( '}' );
    buf.append( "[name=" ).append( getName() ); //$NON-NLS-1$
    buf.append( ", axay=" ).append( getAxAy() ); //$NON-NLS-1$
    buf.append( ", dp=" ).append( getDp() ); //$NON-NLS-1$
    buf.append( ", eddy_xx=" ).append( getEddyXX() ); //$NON-NLS-1$
    buf.append( ", eddy_yx=" ).append( getEddyYX() ); //$NON-NLS-1$
    buf.append( ", eddy_xy=" ).append( getEddyXY() ); //$NON-NLS-1$
    buf.append( ", eddy_yy=" ).append( getEddyYY() ); //$NON-NLS-1$
    buf.append( ", ks=" ).append( getKs() ); //$NON-NLS-1$
    buf.append( ", marsh=" ).append( getMarsh() ); //$NON-NLS-1$
    buf.append( ']' );
    return buf.toString();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls#getColorStyle()
   */
  public RGB getColorStyle( )
  {
    return (RGB) getFeature().getProperty( KalypsoModelRoughnessConsts.WBR_PROP_COLOR_STYLE );
  }

}
