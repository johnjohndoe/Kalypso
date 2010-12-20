package org.kalypso.kalypsosimulationmodel.core.roughness;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;

/**
 * This class represents a wbr:roughnessClsCorrection element.
 *
 * @author Patrice Congo
 */
public class RoughnessClsCorrection extends AbstractFeatureBinder implements IRoughnessClsCorrection
{
  public RoughnessClsCorrection( final Feature feature )
  {
    super( feature, KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS_CORRECTION );
  }

  // /**
  // * Create a {@link RoughnessClsCorrection} object based on a wbr:RoughnessClsCorrection feature created as child in
  // * the given parent and link to it by a property which type is specified by the given QName
  // *
  // * @param parentFeature
  // * the parent feature
  // * @param propName
  // * the link property Q-Name
  // */
  // public RoughnessClsCorrection( final Feature parentFeature, final QName propQName )
  // {
  // Assert.throwIAEOnNull( propQName, Messages.getString( "RoughnessClsCorrection.0" ) ); //$NON-NLS-1$
  // Assert.throwIAEOnNull( parentFeature, Messages.getString( "RoughnessClsCorrection.1" ) ); //$NON-NLS-1$
  // try
  // {
  // this.feature = FeatureHelper.addFeature( parentFeature, propQName,
  // KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS_CORRECTION );
  //
  // }
  // catch( final GMLSchemaException ex )
  // {
  //
  // throw new IllegalArgumentException( "Property " + propQName + //$NON-NLS-1$
  // Messages.getString( "RoughnessClsCorrection.3" ) + //$NON-NLS-1$
  // KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS, ex );
  // }
  // }

  /*
   * (non-Javadoc)
   *
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#configure(double, double, double,
   *      double)
   */
  @Override
  public RoughnessCorConfigConsistency configure( final double ksCor, final double axayCor, final double dpCor, final double eddyCor, final double marshCor )
  {
    final RoughnessCorConfigConsistency check = RoughnessClsCorrection.validate( ksCor, axayCor, dpCor, eddyCor, marshCor );
    if( check != RoughnessCorConfigConsistency.OK )
    {
      return check;
    }
    setKsCor( ksCor );
    setAxAyCor( axayCor );
    setDpCor( dpCor );
    setKsCor( ksCor );
    setMarshCor( marshCor );
    return RoughnessCorConfigConsistency.OK;
  }

  public static final RoughnessCorConfigConsistency validate( final double ksCor, final double axayCor, final double dpCor, final double eddyCor, final double marshCor )
  {
    if( Double.isNaN( axayCor ) || axayCor < 0 )
      return RoughnessCorConfigConsistency.ILLEGAL_VALUE_AXAY_COR;

    if( Double.isNaN( dpCor ) || dpCor < 0 )
      return RoughnessCorConfigConsistency.ILLEGAL_VALUE_DP_COR;

    if( Double.isNaN( eddyCor ) || eddyCor < 0 )
      return RoughnessCorConfigConsistency.ILLEGAL_VALUE_EDDY_COR;

    if( Double.isNaN( ksCor ) || ksCor < 0 )
      return RoughnessCorConfigConsistency.ILLEGAL_VALUE_KS_COR;

    if( Double.isNaN( marshCor ) || marshCor < 0 )
      return RoughnessCorConfigConsistency.ILLEGAL_VALUE_MARSH_COR;

    return RoughnessCorConfigConsistency.OK;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getAxAyCor()
   */
  @Override
  public double getAxAyCor( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  @Override
  public double getMarshCor( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_MARSH_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getDpCor()
   */
  @Override
  public double getDpCor( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getEddyCor()
   */
  @Override
  public double getEddyCor( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#getKsCor()
   */
  @Override
  public double getKsCor( )
  {
    final Double property = getProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setAxAyCor(double)
   */
  @Override
  public void setAxAyCor( final double axayCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( axayCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.6" ) ); //$NON-NLS-1$
    getFeature().setProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY_COR, axayCor );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setDpCor(double)
   */
  @Override
  public void setDpCor( final double dpCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( dpCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.7" ) ); //$NON-NLS-1$
    getFeature().setProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP_COR, dpCor );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setEddy(double)
   */
  @Override
  public void setEddyCor( final double eddyCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( eddyCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.8" ) ); //$NON-NLS-1$
    getFeature().setProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY_COR, eddyCor );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setKsCor(double)
   */
  @Override
  public void setKsCor( final double ksCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( ksCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.9" ) ); //$NON-NLS-1$
    getFeature().setProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS_COR, ksCor );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#setMarshCor(double)
   */
  @Override
  public void setMarshCor( final double marshCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( marshCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.10" ) ); //$NON-NLS-1$
    getFeature().setProperty( KalypsoModelRoughnessConsts.WBR_PROP_MARSH_COR, marshCor );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCorrection#validate()
   */
  @Override
  public RoughnessCorConfigConsistency validate( )
  {
    final double axayCor = getAxAyCor();
    final double ksCor = getKsCor();
    final double dpCor = getDpCor();
    final double eddyCor = getDpCor();
    final double marshCor = getMarshCor();

    return RoughnessClsCorrection.validate( ksCor, axayCor, dpCor, eddyCor, marshCor );
  }

  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 64 );
    buf.append( "RoughnessClsCorrection" ); //$NON-NLS-1$
    final String id = getFeature().getId();
    if( id != null )
    {
      buf.append( '.' );
      buf.append( id );
    }
    buf.append( '[' );
    buf.append( "axayCor=" ); //$NON-NLS-1$
    buf.append( getAxAyCor() );

    buf.append( ", dpCor=" ); //$NON-NLS-1$
    buf.append( getDpCor() );

    buf.append( ", eddyCor=" ); //$NON-NLS-1$
    buf.append( getEddyCor() );

    buf.append( ",ksCor=" ); //$NON-NLS-1$
    buf.append( getKsCor() );

    buf.append( ",marshCor=" ); //$NON-NLS-1$
    buf.append( getMarshCor() );

    buf.append( ' ' );
    buf.append( ']' );

    return buf.toString();
  }

  /**
   * TODO: DANGEROUS: hashCode is NOT implemented!<br>
   *
   * @see org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof IRoughnessClsCorrection )
    {
      final IRoughnessClsCorrection cor = (IRoughnessClsCorrection) obj;

      // TODO: DANGEROUS: double comparisons... Do not use!
      if( Double.compare( cor.getAxAyCor(), getAxAyCor() ) != 0 )
      {
        return false;
      }
      if( Double.compare( cor.getDpCor(), getDpCor() ) != 0 )
      {
        return false;
      }
      if( Double.compare( cor.getEddyCor(), getEddyCor() ) != 0 )
      {
        return false;
      }
      if( Double.compare( cor.getKsCor(), getKsCor() ) != 0 )
      {
        return false;
      }
      if( Double.compare( cor.getMarshCor(), getMarshCor() ) != 0 )
      {
        return false;
      }
      return true;
    }
    else
    {
      return super.equals( obj );
    }
  }

}
