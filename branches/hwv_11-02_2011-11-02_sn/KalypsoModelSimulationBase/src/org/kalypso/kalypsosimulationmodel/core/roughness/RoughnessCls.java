package org.kalypso.kalypsosimulationmodel.core.roughness;

import javax.xml.namespace.QName;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.afgui.model.Util;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

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
   * @param feature
   *          - the wbr:Roughness feature to wrapped
   * @throw IllegalArgumentException if feature is null or is note a wbr:Roughness feature
   */
  public RoughnessCls( final Feature featureToBind ) throws IllegalArgumentException
  {
    super( featureToBind, KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
  }

  /**
   * This constructor creates {@link RoughnessCls} based on a wbr:RoughnessCls feature which is created as child of the
   * given parent feaure and linked to it by the property of the type specified by the argument propQName.
   *
   * @param parentFeature
   *          the parent feature for the new wbr:Roughness class
   * @param propQName
   *          the Q-name of the linking property type
   * @throws IllegalArgumentException
   *           if workspace is null or the roughness collection is not part of the workspace
   */
  public RoughnessCls( final Feature parentFeature, final QName propQName ) throws IllegalArgumentException
  {
    super( Util.createFeatureAsProperty( parentFeature, propQName, KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS ), KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getAxAy()
   */
  @Override
  public double getAxAy( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY, Double.class );
    if( property == null )
      return 0.0;

    return property;
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getDp()
   */
  @Override
  public double getDp( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP, Double.class );
    if( property == null )
      return 0.0;

    return property;
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getEddy()
   */
  @Override
  public double getEddyXX( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XX, Double.class );
    if( property == null )
      return 0.0;

    return property;
  }

  @Override
  public double getEddyYX( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YX, Double.class );
    if( property == null )
      return 0.0;

    return property;
  }

  @Override
  public double getEddyXY( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XY, Double.class );
    if( property == null )
      return 0.0;

    return property;
  }

  @Override
  public double getEddyYY( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YY, Double.class );
    if( property == null )
      return 0.0;

    return property;
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getKs()
   */
  @Override
  public double getKs( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  @Override
  public double getMarsh( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setAxAy(double)
   */
  @Override
  public void setAxAy( final double axay ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY, axay );
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setDp(double)
   */
  @Override
  public void setDp( final double dp ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP, dp );
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setEddy(double)
   */
  @Override
  public void setEddyXX( final double eddy ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XX, eddy );
  }

  @Override
  public void setEddyYX( final double eddy ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YX, eddy );
  }

  @Override
  public void setEddyXY( final double eddy ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XY, eddy );
  }

  @Override
  public void setEddyYY( final double eddy ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YY, eddy );
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setKs(double)
   */
  @Override
  public void setKs( final double ks ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS, ks );
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setMarsh(double)
   */
  @Override
  public void setMarsh( final double marsh ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV, marsh );
  }

  // /Ask nico for details
  // one is sure negatives are not allowed
  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#validate()
   */
  @Override
  public RoughnessConfigConsistency validate( )
  {
    return null;
  }

  /*
   * (non-Javadoc)
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#configure(java.lang.String, double, double,
   * double, double)
   */
  @Override
  public RoughnessConfigConsistency configure( final String name, final double ks, final double axay, final double dp, final double eddy_xx, final double eddy_yx, final double eddy_xy, final double eddy_yy, final double marsh )
  {
    final RoughnessConfigConsistency check = validate( name, ks, axay, dp, eddy_xx, eddy_yx, eddy_xy, eddy_yy, marsh );
    if( check != RoughnessConfigConsistency.OK )
      return check;

    final Feature feature = getFeature();
    feature.setProperty( Feature.QN_NAME, name );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS, ks );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP, dp );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XX, eddy_xx );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YX, eddy_yx );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XY, eddy_xy );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YY, eddy_yy );
    feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY, axay );

    return RoughnessConfigConsistency.OK;
  }

  public static RoughnessConfigConsistency validate( final String name, final double ks, final double axay, final double dp, final double eddy_xx, final double eddy_yx, final double eddy_xy, final double eddy_yy, final double marsh )
  {
    if( Assert.isNullOrEmpty( name ) )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_NAME;

    if( ks < 0 )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_KS;

    if( eddy_xx <= 0 )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_EDDY_XX;

    if( eddy_yx <= 0 )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_EDDY_YX;

    if( eddy_xy <= 0 )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_EDDY_XY;

    if( eddy_yy <= 0 )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_EDDY_YY;

    if( dp < 0 )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_DP;

    if( axay < 0 )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_AXAY;

    if( marsh < 0 )
      return RoughnessConfigConsistency.ILLEGAL_VALUE_MARSH;

    return RoughnessConfigConsistency.OK;
  }

  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 64 );
    buf.append( "RoughnessCls" ); //$NON-NLS-1$
    final String id = getGmlID();
    if( id != null )
      buf.append( '{' ).append( id ).append( '}' );
    buf.append( "[name=" ).append( getName() ); //$NON-NLS-1$
    buf.append( ", axay=" ).append( getAxAy() ); //$NON-NLS-1$
    buf.append( ", dp=" ).append( getDp() ); //$NON-NLS-1$
    buf.append( ", ks=" ).append( getKs() ); //$NON-NLS-1$
    buf.append( ", marsh=" ).append( getMarsh() ); //$NON-NLS-1$
    buf.append( ']' );
    return buf.toString();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls#getColorStyle()
   */
  @Override
  public RGB getColorStyle( )
  {
    return (RGB) getFeature().getProperty( KalypsoModelRoughnessConsts.WBR_PROP_COLOR_STYLE );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.modeling.IColorStyledFeatureWrapper#getOrdinalNumber()
   */
  @Override
  public int getOrdinalNumber( )
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.modeling.IColorStyledFeatureWrapper#setColorStyle(org.eclipse.swt.graphics.RGB)
   */
  @Override
  public void setColorStyle( final RGB rgb )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls#getViscosities()
   */
  @Override
  public Double[] getViscosities( final int iedsw )
  {
    final Double[] result = new Double[4];

    /*
     * turbulence combo (iedsw), these values mean: constant eddy viscosity to be applied, but value has to come from
     * the class or it will be the default value.
     */
    if( iedsw == 0 || iedsw == 10 || iedsw == 13 )
    {
      /*
       * Get the default eddy value from the getEddy-method, too. There the information is given, whether it has some
       * value or default should be applied. Furthermore value is changed to 2500.
       */
      result[0] = getEddyXX();
      result[1] = getEddyYX();
      result[2] = getEddyXY();
      result[3] = getEddyYY();
    }
    else
    {
      /*
       * For usage of non-constant eddy-viscosity approaches, the value is 0.5
       */
      final double defaultValue = 0.5;
      result[0] = defaultValue;
      result[1] = defaultValue;
      result[2] = defaultValue;
      result[3] = defaultValue;
    }
    return result;
  }
}
