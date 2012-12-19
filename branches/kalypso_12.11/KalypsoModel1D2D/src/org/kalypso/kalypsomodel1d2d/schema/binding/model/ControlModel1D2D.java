/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.schema.binding.model;

import java.io.File;
import java.math.BigInteger;
import java.net.URL;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.FileLocator;
import org.kalypso.afgui.model.Util;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * @author Patrice Congo
 * @author Madanagopal
 * @author Dejan Antanaskovic
 */
public class ControlModel1D2D extends Feature_Impl implements IControlModel1D2D
{
  public ControlModel1D2D( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private final IFeatureBindingCollection<IRestartInfo> m_restartInfos = new FeatureBindingCollection<>( this, IRestartInfo.class, QNAME_PROPERTY_RESTART_INFO );

  public final static QName WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "timestepsMember" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_UNSTEADY_CHECKBOX = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_unsteady" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_STEADY_CHECKBOX = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_steady" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_RELAXATION_FACTOR = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_steadyBC" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_P_BOTTOM = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_p_bottom" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_FIXEDMARSHBOTTOM = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "FIXEDMARSHBOTTOM" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_AC4 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC4" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_AC3 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC3" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_AC2 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC2" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_AC1 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC1" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_MARSHFRICTIONFACTOR = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "MARSHFRICTIONFACTOR" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_MARSHFRICTIONDISTR = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "MARSHFRICTIONDISTR" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_BEIENT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "BEIENT" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_ICPU = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ICPU" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_MFW = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "MFW" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_BUFFSIZ = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "BUFFSIZ" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_PERCENT_CHECK = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "PERCENT_CHECK" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_VEGETA = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "VEGETA" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_CHI = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "CHI" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_HASWINDDRAG = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "HASWINDDRAG" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_RESTART = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_restart" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_IACCYC = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "IACCYC" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_DRFACT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "DRFACT" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_IDRPT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "IDRPT" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_CONV_3 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "CONV_3" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_CONV_2 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "CONV_2" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_CONV_1 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "CONV_1" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_NITN = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "NITN" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_NITI = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "NITI" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_DSETD = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "DSETD" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_DSET = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "DSET" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_HMIN = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "HMIN" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_UNOM = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "UNOM" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_UDIR = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "UDIR" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_ELEV = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ELEV" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_OMEGA = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "OMEGA" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_TBMIN = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "TBMIN" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_TBFACT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "TBFACT" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_TBFACT_ESCUDIER = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "TBFACT_ESCUDIER" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_IEDSW = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "IEDSW" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_IDNOPT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "IDNOPT" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_STARTSIM = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "startsim" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_VERSION = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "Version" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_F_MODEL = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ControlModel" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_F_MODEL_SWAN = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANCalculation" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_VERSION_SWAN = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANVersion" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_SWAN_BOUNDARY_CONST = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANConstantBoundary" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_SWAN_BOUNDARY_ALG = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANBoundaryAlg" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_INITialValues = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANINITialValues" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_INITialValuesPar = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANINITialValuesPar" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_ConstantWind = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANConstantWind" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_ConstantWindPar = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANConstantWindPar" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_AdditionalResultsPar = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANAdditionalResultsPar" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_COORD_CART = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANCoordCart" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_GEN3 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANGEN3" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_INPUT_COORD_FILE = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANInputCoordFile" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_SWAN_INPUT_ADDITIONAL_COMMANDS = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "SWANInputAdditionalCmds" ); //$NON-NLS-1$

  @Override
  public IObservation<TupleResult> getTimeSteps( )
  {
    try
    {
      final Feature feature = this;
      final Object property = feature.getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
      return (IObservation<TupleResult>)((Feature)property).getAdapter( IObservation.class );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }

  @Override
  public Integer getIDNOPT( )
  {
    return (Integer)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_IDNOPT );
  }

  @Override
  public String getVersion( )
  {
    return (String)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_VERSION );
  }

  @Override
  public int getStartYear( )
  {
    return getStartCalendar().getYear();
  }

  @Override
  public int getIaccyc( )
  {
    // REMARK: do not change the value here, this is DANGEROUS!
    // This class is just the binding class and should not decide what to do about its data...

    final BigInteger propertyValue = getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_IACCYC, BigInteger.class );

    // DANGEROUS: does the user get some warning, that he has chosen restart but it is ignored? I think not...
    if( propertyValue != null && getRestart() && !isSteadySelected() && isUnsteadySelected() )
      return propertyValue.intValue();

    return 0;
  }

  @Override
  public Integer getIcpu( )
  {
    final Integer propertyValue = (Integer)getProperty( WB1D2DCONTROL_PROP_ICPU );
    return propertyValue != null ? propertyValue : 0;
  }

  @Override
  public Integer getMFW( )
  {
    final Integer propertyValue = (Integer)getProperty( WB1D2DCONTROL_PROP_MFW );
    return propertyValue != null ? propertyValue : 0;
  }

  @Override
  public Integer getBUFFSIZ( )
  {
    final Integer propertyValue = (Integer)getProperty( WB1D2DCONTROL_PROP_BUFFSIZ );
    return propertyValue != null ? propertyValue : 0;
  }

  @Override
  public boolean getPercentCheck( )
  {
    final Boolean propertyValue = (Boolean)getProperty( WB1D2DCONTROL_PROP_PERCENT_CHECK );
    return propertyValue != null ? propertyValue : false;
  }

  @Override
  public boolean getRestart( )
  {
    return ((Boolean)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_RESTART )).booleanValue();
  }

  @Override
  public List<IRestartInfo> getRestartInfos( )
  {
    return m_restartInfos;
  }

  @Override
  public XMLGregorianCalendar getStartCalendar( )
  {
    return (XMLGregorianCalendar)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_STARTSIM );
  }

  @Override
  public Integer getStartJulianDay( )
  {
    final GregorianCalendar calendar = getStartCalendar().toGregorianCalendar();
    return calendar.get( Calendar.DAY_OF_YEAR );
  }

  @Override
  public Double getStartHour( )
  {
    return new Double( getStartCalendar().getHour() );
  }

  @Override
  public Integer getIEDSW( )
  {
    final Integer property = (Integer)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_IEDSW );
    return property != null ? property : 0;
  }

  @Override
  public Double getTBFACT( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TBFACT );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getTBFACT_ESCUDIER( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TBFACT_ESCUDIER );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getTBMIN( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TBMIN );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getOMEGA( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_OMEGA );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getELEV( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_ELEV );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getUDIR( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_UDIR );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getUNOM( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_UNOM );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getHMIN( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_HMIN );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getDSET( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_DSET );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getDSETD( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_DSETD );
    return property != null ? property : 0.0;
  }

  @Override
  public Integer getNITI( )
  {
    if( !isSteadySelected() )
      return 0;
    Integer property = (Integer)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_NITI );
    if( property == null )
      property = 0;
    return property;
  }

  @Override
  public Integer getNITN( )
  {
    if( !isUnsteadySelected() )
      return 0;
    Integer property = (Integer)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_NITN );
    if( property == null )
      property = 0;
    return property;
  }

  @Override
  public Integer getNCYC( )
  {
    if( !isUnsteadySelected() )
      return 0;
    final Integer property = getTimeSteps().getResult().size() - 1;
    return getNITN() != null ? property : 0; // Not needed, while no definition of unsteady timesteps
  }

  @Override
  public Double getCONV_1( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_CONV_1 );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getCONV_2( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_CONV_2 );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getCONV_3( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_CONV_3 );
    return property != null ? property : 0.0;
  }

  @Override
  public Integer getIDRPT( )
  {
    final Integer property = (Integer)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_IDRPT );
    return property != null ? property : 0;
  }

  @Override
  public Double getDRFACT( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_DRFACT );
    return property != null ? property : 0.0;
  }

  @Override
  public boolean getVegeta( )
  {
    return (Boolean)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_VEGETA );
  }

  @Override
  public boolean getHasWindDrag( )
  {
    final Boolean property = getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_HASWINDDRAG, Boolean.class );
    if( property == null )
      return false;
    return property;
  }

  @Override
  public Double getChi( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_CHI );
    return property != null ? property : 0.0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getBeient()
   */
  @Override
  public boolean getBeient( )
  {
    final Object property = getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_BEIENT );
    return property != null ? (Boolean)property : false;
  }

  @Override
  public boolean getFixedMarshBottom( )
  {
    final Boolean property = (Boolean)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_FIXEDMARSHBOTTOM );
    return property != null ? property : false;
  }

  @Override
  public Double getAC1( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_AC1 );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getAC2( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_AC2 );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getAC3( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_AC3 );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getAC4( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_AC4 );
    return property != null ? property : 0.0;
  }

  @Override
  public Double getMarshFrictionFactor( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_MARSHFRICTIONFACTOR );
    return property != null ? property : 20.0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getMarshFactor()
   */
  @Override
  public Integer getMarshFrictionDistr( )
  {
    final Integer property = (Integer)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_MARSHFRICTIONDISTR );
    return property != null ? property : 2;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#setCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  @Override
  public void setCalculationUnit( final ICalculationUnit calUnit )
  {
    try
    {
      final GMLWorkspace workspace = calUnit.getWorkspace();
      final URL context = workspace.getContext();
      final File file = new File( FileLocator.resolve( context ).getFile() );
      final String name = file.getName();
      final String href = name + "#" + calUnit.getId(); //$NON-NLS-1$

      setLink( ICalculationUnit1D2D.WB1D2D_PROP_CALC_UNIT, href, calUnit.getFeatureType() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( "Exception while setting calculation unit link to control" ); //$NON-NLS-1$
    }
  }

  @Override
  public ICalculationUnit getCalculationUnit( )
  {
    try
    {
      final ICalculationUnit resolveLink = FeatureHelper.resolveLink( this, ICalculationUnit1D2D.WB1D2D_PROP_CALC_UNIT, ICalculationUnit.class );
      return resolveLink;
    }
    catch( final IllegalStateException e )
    {
      // appends if the gml are not saved yet
      // hacking by getting the feature id and returning
      // the one id the control workspace with the same id
      // this will only work in the same scenario context
      final Feature resolveLink = FeatureHelper.resolveLink( this, ICalculationUnit1D2D.WB1D2D_PROP_CALC_UNIT );
      if( !(resolveLink instanceof IXLinkedFeature) )
      {
        throw e;
      }
      final String featureId = ((IXLinkedFeature)resolveLink).getFeatureId();
      final IFEDiscretisationModel1d2d model = Util.getModel( IFEDiscretisationModel1d2d.class.getName() );
      if( model == null )
      {
        throw e;
      }
      final GMLWorkspace modelWorkspace = model.getWorkspace();
      final Feature calUnitFeature = modelWorkspace.getFeature( featureId );
      if( calUnitFeature == null )
      {
        return null;
      }
      final ICalculationUnit adapter = (ICalculationUnit)calUnitFeature.getAdapter( ICalculationUnit.class );
      return adapter;
    }
  }

  @Override
  public Double get_P_BOTTOM( )
  {
    final Double property = (Double)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_P_BOTTOM );
    return property != null ? property : 0.0;
  }

  @Override
  public String get_RelaxationsFactor( )
  {
    return (String)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_RELAXATION_FACTOR );
  }

  @Override
  public boolean isSteadySelected( )
  {
    final Boolean property = (Boolean)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_STEADY_CHECKBOX );
    return property != null ? property.booleanValue() : false;
  }

  @Override
  public boolean isUnsteadySelected( )
  {
    final Boolean property = (Boolean)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_UNSTEADY_CHECKBOX );
    return property != null ? property.booleanValue() : false;
  }

  @Override
  public IRestartInfo addRestartInfo( )
  {
    return m_restartInfos.addNew( IRestartInfo.QNAME, IRestartInfo.class );
  }

  @Override
  public boolean calculateSWAN( )
  {
    final Object swan = getProperty( ControlModel1D2D.WB1D2DCONTROL_F_MODEL_SWAN );
    if( swan == null )
      return false;
    return ((Boolean)swan).booleanValue();
  }

  @Override
  public String getVersionSWAN( )
  {
    return (String)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_VERSION_SWAN );
  }

  @Override
  public boolean isRestartAfterSWAN( )
  {
    // TODO: implement coupled restart, if somebody will really need it... :)
    // try
    // {
    // return ((Boolean) getProperty( ControlModel1D2D.WB1D2DCONTROL_F_RESTART_MODEL_SWAN ));
    // }
    // catch( Exception e )
    // {
    // return false;
    // }
    return false;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#setCalculateSWAN(boolean)
   */
  @Override
  public void setCalculateSWAN( final boolean doCalculateSWAN )
  {
    setProperty( ControlModel1D2D.WB1D2DCONTROL_F_MODEL_SWAN, doCalculateSWAN );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getINITialValues()
   */
  @Override
  public Integer getINITialValuesSWAN( )
  {
    final Integer property = (Integer)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_SWAN_INITialValues );
    return property != null ? property : 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getINITialValuesPar()
   */
  @Override
  public String getINITialValuesParSWAN( )
  {
    return (String)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_SWAN_INITialValuesPar );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getSWAN_BOUNDARY_ALG()
   */
  @Override
  public Integer getAlgBoundarySWAN( )
  {
    final Integer property = (Integer)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_SWAN_SWAN_BOUNDARY_ALG );
    return property != null ? property : 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#isCOORD_CART()
   */
  @Override
  public boolean isInCartCoordSWAN( )
  {
    final Boolean propertyValue = (Boolean)getProperty( WB1D2DCONTROL_PROP_SWAN_COORD_CART );
    return propertyValue != null ? propertyValue : false;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#isConstantWind()
   */
  @Override
  public boolean isConstantWindSWAN( )
  {
    final Boolean propertyValue = (Boolean)getProperty( WB1D2DCONTROL_PROP_SWAN_ConstantWind );
    return propertyValue != null ? propertyValue : false;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#isGEN3()
   */
  @Override
  public boolean isWindGEN3SWAN( )
  {
    final Boolean propertyValue = (Boolean)getProperty( WB1D2DCONTROL_PROP_SWAN_GEN3 );
    return propertyValue != null ? propertyValue : false;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getConstantWindPar()
   */
  @Override
  public String getConstantWindParSWAN( )
  {
    return (String)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_SWAN_ConstantWindPar );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getSWANAdditionalResultsPar()
   */
  @Override
  public String getAdditionalResultsParSWAN( )
  {
    return (String)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_SWAN_AdditionalResultsPar );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getAdditionalSimParSWAN()
   */
  @Override
  public String getAdditionalSimParSWAN( )
  {
    return (String)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_SWAN_INPUT_ADDITIONAL_COMMANDS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getInputFileAdditionalCoordSWAN()
   */
  @Override
  public String getInputFileAdditionalCoordSWAN( )
  {
    return (String)getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_SWAN_INPUT_COORD_FILE );
  }

}
