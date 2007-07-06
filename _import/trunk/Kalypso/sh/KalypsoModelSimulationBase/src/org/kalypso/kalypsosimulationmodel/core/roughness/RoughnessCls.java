package org.kalypso.kalypsosimulationmodel.core.roughness;

import javax.xml.namespace.QName;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * Default implementation for {@link IRoughnessCls} which serves as a wrapper arround a wbd:Roughness feature
 * 
 * @author Patrice Congo
 */
public class RoughnessCls 
                extends AbstractFeatureBinder 
                implements IRoughnessCls
{
//  private final Feature m_Feature;

  /**
   * Create a Roughness object wrapping the given feature.
   * 
   * @param feature -
   *          the wbr:Roughness feature to wrapped
   * @throw IllegalArgumentException if feature is null or is note a wbr:Roughness feature
   */
  public RoughnessCls( Feature featureToBind ) throws IllegalArgumentException
  {
//    Assert.throwIAEOnNotDirectInstanceOf( feature, KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
//    this.m_Feature = feature;
    super(  featureToBind, 
            KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
    
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
  public RoughnessCls( Feature parentFeature, QName propQName ) throws IllegalArgumentException
  {
//    Assert.throwIAEOnNull( propQName, "Argument propQName must not be null" );
//    Assert.throwIAEOnNull( parentFeature, "Argument roughnessCollection must not be null" );
//    try
//    {
//      this.m_Feature = FeatureHelper.addFeature( parentFeature, propQName, KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
//
//    }
//    catch( GMLSchemaException ex )
//    {
//
//      throw new IllegalArgumentException( "Property " + propQName + " does not accept element of type" + KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS, ex );
//    }
    super(
        Util.createFeatureAsProperty( 
                parentFeature, 
                propQName, 
                KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS )  
        /*FeatureHelper.addFeature( 
              parentFeature, 
              propQName,
              KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS)*/, 
          KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
        
  }

  
  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getAxAy()
   */
  public double getAxAy( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( 
                feature, 
                KalypsoModelRoughnessConsts.WBR_PROP_AXAY, 
                Double.NaN );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getDp()
   */
  public double getDp( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( 
              feature, 
              KalypsoModelRoughnessConsts.WBR_PROP_DP, 
              Double.NaN );
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
    return FeatureHelper.getAsDouble( 
                feature, 
                KalypsoModelRoughnessConsts.WBR_PROP_KS, 
                Double.NaN );
  }

  public double getMarsh( )
  {
    final Feature feature = getFeature();
    return FeatureHelper.getAsDouble( 
            feature, 
            KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV, Double.NaN );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setAxAy(double)
   */
  public void setAxAy( double axay ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( 
        KalypsoModelRoughnessConsts.WBR_PROP_AXAY, Double.valueOf( axay ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setDp(double)
   */
  public void setDp( double dp ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( 
        KalypsoModelRoughnessConsts.WBR_PROP_DP, Double.valueOf( dp ) );
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
    feature.setProperty( 
        KalypsoModelRoughnessConsts.WBR_PROP_KS, Double.valueOf( ks ) );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setMarsh(double)
   */
  public void setMarsh( double marsh ) throws IllegalArgumentException
  {
    final Feature feature = getFeature();
    feature.setProperty( 
        KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV, Double.valueOf( marsh ) );
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

//  /*
//   * (non-Javadoc)
//   * 
//   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getName()
//   */
//  public String getName( )
//  {
//    Object obj = m_Feature.getProperty( KalypsoModelRoughnessConsts.GML_PROP_NAME );
//    if( obj instanceof String )
//    {
//      return (String) obj;
//    }
//    else if( obj instanceof List )
//    {
//      if( ((List) obj).size() > 0 )
//      {
//        return (String) ((List) obj).get( 0 );
//      }
//      else
//      {
//        return null;
//      }
//    }
//    else
//    {
//      return null;
//    }
//  }

//  /*
//   * (non-Javadoc)
//   * 
//   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getURI()
//   */
//  public String getURI( )
//  {
//    // return feature.getId();
//    Object obj = m_Feature.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_URI );
//    if( obj instanceof String )
//    {
//      return (String) obj;
//    }
//    else
//    {
//      return null;
//    }
//  }
//
//  /*
//   * (non-Javadoc)
//   * 
//   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setName(java.lang.String)
//   */
//  public void setName( String name ) throws IllegalArgumentException
//  {
//    name = Assert.throwIAEOnNullOrEmpty( name );
//    // FeatureHelper.addProperty(
//    // feature,
//    // KalypsoModelRoughnessConsts.GML_PROP_NAME,
//    // Arrays.asList(new String[]{name}) );
//    m_Feature.setProperty( KalypsoModelRoughnessConsts.GML_PROP_NAME, Arrays.asList( new String[] { name } ) );
//  }

//  /*
//   * (non-Javadoc)
//   * 
//   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setURI(java.lang.String)
//   */
//  public void setURI( String uri ) throws IllegalArgumentException
//  {
//    uri = Assert.throwIAEOnNullOrEmpty( uri );
//    if( uri.equals( m_Feature.getId() ) )
//    {
//      return;
//    }
//    // TODO test the workspace for uri not already in use
//    // GMLWorkspace workspace=feature.getWorkspace();
//    // FeatureVisitor vi
//    // uri ist treated as the gml:id
//
//    if( Util.isInFeatureWorkspace( m_Feature, KalypsoModelRoughnessConsts.WBR_PROP_URI, uri ) )
//    {
//      throw new IllegalArgumentException( "uri already in workspace:" + uri );
//    }
//
//    m_Feature.setProperty( KalypsoModelRoughnessConsts.WBR_PROP_URI, uri );
//
//  }

//  /*
//   * (non-Javadoc)
//   * 
//   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getDescription()
//   */
//  public String getDescription( )
//  {
//    Object obj = m_Feature.getProperty( KalypsoModelRoughnessConsts.GML_PROP_DESCRIPTION );
//    if( obj instanceof String )
//    {
//      return (String) obj;
//    }
//    else if( obj == null )
//    {
//      return null;
//    }
//    else
//    {
//      throw new RuntimeException( "Unexpected property value:" + obj + "\n\tof type:" + obj.getClass() );
//    }
//  }

//  /*
//   * (non-Javadoc)
//   * 
//   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setDescription(java.lang.String)
//   */
//  public void setDescription( String descriptionText )
//  {
//    Assert.throwIAEOnNull( descriptionText, null );
//    m_Feature.setProperty( KalypsoModelRoughnessConsts.GML_PROP_DESCRIPTION, descriptionText );
//  }

//  public Feature getWrappedFeature( )
//  {
//    return m_Feature;
//  }

//  /**
//   * @see org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper#getGmlID()
//   */
//  public String getGmlID( )
//  {
//    return m_Feature.getId();
//  }

  @Override
  public String toString( )
  {
    StringBuffer buf = new StringBuffer( 64 );
    buf.append( "RoughnessCls" );
    String id = getGmlID();
    if( id != null )
    {
      buf.append( '{' );
      buf.append( id );
      buf.append( '}' );
    }
    buf.append( '[' );
    buf.append( "name=" );
    buf.append( getName() );

    buf.append( "axay=" );
    buf.append( getAxAy() );

    buf.append( ", dp=" );
    buf.append( getDp() );

    buf.append( ", eddy_xx=" );
    buf.append( getEddyXX() );

    buf.append( ", eddy_yx=" );
    buf.append( getEddyYX() );

    buf.append( ", eddy_xy=" );
    buf.append( getEddyXY() );

    buf.append( ", eddy_yy=" );
    buf.append( getEddyYY() );

    buf.append( ",ks=" );
    buf.append( getKs() );

    buf.append( ",marsh=" );
    buf.append( getMarsh() );

    buf.append( ' ' );
    buf.append( ']' );

    return buf.toString();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls#getColorStyle()
   */
  public RGB getColorStyle( )
  {
    final Feature feature = getFeature();
    RGB property = 
      (RGB) feature.getProperty( 
              KalypsoModelRoughnessConsts.WBR_PROP_COLOR_STYLE );
    return property;
  }
}
