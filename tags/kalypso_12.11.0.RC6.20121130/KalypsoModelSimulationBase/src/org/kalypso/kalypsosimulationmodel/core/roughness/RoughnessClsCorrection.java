package org.kalypso.kalypsosimulationmodel.core.roughness;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * FIXME: never used <br/>
 * This class represents a wbr:roughnessClsCorrection element.
 *
 * @author Patrice Congo
 */
public class RoughnessClsCorrection extends Feature_Impl implements IRoughnessClsCorrection
{
  public RoughnessClsCorrection( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

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

  @Override
  public double getAxAyCor( )
  {
    final Double property = getProperty( WBR_PROP_AXAY_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  @Override
  public double getMarshCor( )
  {
    final Double property = getProperty( WBR_PROP_MARSH_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  @Override
  public double getDpCor( )
  {
    final Double property = getProperty( WBR_PROP_DP_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  @Override
  public double getEddyCor( )
  {
    final Double property = getProperty( WBR_PROP_EDDY_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  @Override
  public double getKsCor( )
  {
    final Double property = getProperty( WBR_PROP_KS_COR, Double.class );
    if( property == null )
      return Double.NaN;

    return property;
  }

  @Override
  public void setAxAyCor( final double axayCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( axayCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.6" ) ); //$NON-NLS-1$
    setProperty( WBR_PROP_AXAY_COR, axayCor );
  }

  @Override
  public void setDpCor( final double dpCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( dpCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.7" ) ); //$NON-NLS-1$
    setProperty( WBR_PROP_DP_COR, dpCor );
  }

  @Override
  public void setEddyCor( final double eddyCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( eddyCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.8" ) ); //$NON-NLS-1$
    setProperty( WBR_PROP_EDDY_COR, eddyCor );
  }

  @Override
  public void setKsCor( final double ksCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( ksCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.9" ) ); //$NON-NLS-1$
    setProperty( WBR_PROP_KS_COR, ksCor );
  }

  @Override
  public void setMarshCor( final double marshCor ) throws IllegalArgumentException
  {
    Assert.throwIAEOnLessThan0( marshCor, Messages.getString( "org.kalypso.kalypsosimulationmodel.core.roughness.RoughnessClsCorrection.10" ) ); //$NON-NLS-1$
    setProperty( WBR_PROP_MARSH_COR, marshCor );
  }

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
    final String id = getId();
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

  // FIXME 1) overwriting equals of Feature's is forbidden -> break integrity of feature lists etc.
  // FIXME 2) !! hashCode not overwritten as well !!
  // @Override
  // public boolean equals( final Object obj )
  // {
  // if( obj instanceof IRoughnessClsCorrection )
  // {
  // final IRoughnessClsCorrection cor = (IRoughnessClsCorrection) obj;
  //
  // // TODO: DANGEROUS: double comparisons... Do not use!
  // if( Double.compare( cor.getAxAyCor(), getAxAyCor() ) != 0 )
  // {
  // return false;
  // }
  // if( Double.compare( cor.getDpCor(), getDpCor() ) != 0 )
  // {
  // return false;
  // }
  // if( Double.compare( cor.getEddyCor(), getEddyCor() ) != 0 )
  // {
  // return false;
  // }
  // if( Double.compare( cor.getKsCor(), getKsCor() ) != 0 )
  // {
  // return false;
  // }
  // if( Double.compare( cor.getMarshCor(), getMarshCor() ) != 0 )
  // {
  // return false;
  // }
  // return true;
  // }
  // else
  // {
  // return super.equals( obj );
  // }
  // }

}
