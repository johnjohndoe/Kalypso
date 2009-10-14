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
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Patrice Congo
 * @author Madanagopal
 * @author Dejan Antanaskovic
 * 
 */
public class ControlModel1D2D extends AbstractFeatureBinder implements IControlModel1D2D
{
  private final IFeatureWrapperCollection<IRestartInfo> m_restartInfos = new FeatureWrapperCollection<IRestartInfo>( getFeature(), IRestartInfo.class, QNAME_PROPERTY_RESTART_INFO );

  public final static QName WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "timestepsMember" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_UNSTEADY_CHECKBOX = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_unsteady" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_STEADY_CHECKBOX = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_steady" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_RELAXATION_FACTOR = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_steadyBC" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_P_BOTTOM = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "_p_bottom" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_AC3 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC3" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_AC2 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC2" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_AC1 = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "AC1" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_BEIENT = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "BEIENT" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_ICPU = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "ICPU" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_MFW = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "MFW" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_BUFFSIZ = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "BUFFSIZ" ); //$NON-NLS-1$

  public static final QName WB1D2DCONTROL_PROP_PERCENT_CHECK = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "PERCENT_CHECK" ); //$NON-NLS-1$

  public final static QName WB1D2DCONTROL_PROP_VEGETA = new QName( UrlCatalog1D2D.MODEL_1D2DControl_NS, "VEGETA" ); //$NON-NLS-1$

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

  public ControlModel1D2D( final Feature featureToBind )
  {
    this( featureToBind, ControlModel1D2D.WB1D2DCONTROL_F_MODEL );
  }

  public ControlModel1D2D( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getTimeSteps()
   */
  @SuppressWarnings("unchecked")
  public IObservation<TupleResult> getTimeSteps( )
  {
    try
    {
      final Feature feature = getFeature();
      final Object property = feature.getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
      return (IObservation<TupleResult>) ((Feature) property).getAdapter( IObservation.class );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }

  public Integer getIDNOPT( )
  {
    return (Integer) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_IDNOPT );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getVersion()
   */
  public String getVersion( )
  {
    return (String) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_VERSION );
  }

  public int getStartYear( )
  {
    return getStartCalendar().getYear();
  }

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

  public Integer getIcpu( )
  {
    final Integer propertyValue = (Integer) getFeature().getProperty( WB1D2DCONTROL_PROP_ICPU );
    return propertyValue != null ? propertyValue : 0;
  }

  public Integer getMFW( )
  {
    final Integer propertyValue = (Integer) getFeature().getProperty( WB1D2DCONTROL_PROP_MFW );
    return propertyValue != null ? propertyValue : 0;
  }

  public Integer getBUFFSIZ( )
  {
    final Integer propertyValue = (Integer) getFeature().getProperty( WB1D2DCONTROL_PROP_BUFFSIZ );
    return propertyValue != null ? propertyValue : 0;
  }

  public boolean getPercentCheck( )
  {
    final Boolean propertyValue = (Boolean) getFeature().getProperty( WB1D2DCONTROL_PROP_PERCENT_CHECK );
    return propertyValue != null ? propertyValue : false;
  }

  public boolean getRestart( )
  {
    return ((Boolean) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_RESTART )).booleanValue();
  }

  public List<IRestartInfo> getRestartInfos( )
  {
    return m_restartInfos;
  }

  public XMLGregorianCalendar getStartCalendar( )
  {
    return (XMLGregorianCalendar) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_STARTSIM );
  }

  public Integer getStartJulianDay( )
  {
    final GregorianCalendar calendar = getStartCalendar().toGregorianCalendar();
    return calendar.get( Calendar.DAY_OF_YEAR );
  }

  public Double getStartHour( )
  {
    return new Double( getStartCalendar().getHour() );
  }

  public Integer getIEDSW( )
  {
    final Integer property = (Integer) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_IEDSW );
    return property != null ? property : 0;
  }

  public Double getTBFACT( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TBFACT );
    return property != null ? property : 0.0;
  }

  public Double getTBFACT_ESCUDIER( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TBFACT_ESCUDIER );
    return property != null ? property : 0.0;
  }

  public Double getTBMIN( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TBMIN );
    return property != null ? property : 0.0;
  }

  public Double getOMEGA( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_OMEGA );
    return property != null ? property : 0.0;
  }

  public Double getELEV( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_ELEV );
    return property != null ? property : 0.0;
  }

  public Double getUDIR( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_UDIR );
    return property != null ? property : 0.0;
  }

  public Double getUNOM( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_UNOM );
    return property != null ? property : 0.0;
  }

  public Double getHMIN( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_HMIN );
    return property != null ? property : 0.0;
  }

  public Double getDSET( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_DSET );
    return property != null ? property : 0.0;
  }

  public Double getDSETD( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_DSETD );
    return property != null ? property : 0.0;
  }

  public Integer getNITI( )
  {
    if( !isSteadySelected() )
      return 0;
    Integer property = (Integer) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_NITI );
    if( property == null )
      property = 0;
    return property;
  }

  public Integer getNITN( )
  {
    if( !isUnsteadySelected() )
      return 0;
    Integer property = (Integer) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_NITN );
    if( property == null )
      property = 0;
    return property;
  }

  public Integer getNCYC( )
  {
    if( !isUnsteadySelected() )
      return 0;
    final Integer property = getTimeSteps().getResult().size() - 1;
    return getNITN() != null ? property : 0; // Not needed, while no definition of unsteady timesteps
  }

  public Double getCONV_1( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_CONV_1 );
    return property != null ? property : 0.0;
  }

  public Double getCONV_2( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_CONV_2 );
    return property != null ? property : 0.0;
  }

  public Double getCONV_3( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_CONV_3 );
    return property != null ? property : 0.0;
  }

  public Integer getIDRPT( )
  {
    final Integer property = (Integer) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_IDRPT );
    return property != null ? property : 0;
  }

  public Double getDRFACT( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_DRFACT );
    return property != null ? property : 0.0;
  }

  public boolean getVegeta( )
  {
    return (Boolean) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_VEGETA );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getBeient()
   */
  public boolean getBeient( )
  {
    final Object property = getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_BEIENT );
    return property != null ? (Boolean) property : false;
  }

  public Double getAC1( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_AC1 );
    return property != null ? property : 0.0;
  }

  public Double getAC2( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_AC2 );
    return property != null ? property : 0.0;
  }

  public Double getAC3( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_AC3 );
    return property != null ? property : 0.0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#setCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  public void setCalculationUnit( final ICalculationUnit calUnit )
  {
    try
    {
      final Feature wrappedFeature = calUnit.getFeature();
      final Feature parentFeature = getFeature();// control to link to
      final IPropertyType property = parentFeature.getFeatureType().getProperty( ICalculationUnit1D2D.WB1D2D_PROP_CALC_UNIT );

      final GMLWorkspace workspace = calUnit.getFeature().getWorkspace();
      final URL context = workspace.getContext();
      final File filee = new File( FileLocator.resolve( context ).getFile() );
      final String name = filee.getName();// new java.io.File( uri ).getName();
      final XLinkedFeature_Impl linkedFeature = (XLinkedFeature_Impl) FeatureHelper.createLinkToID( name + "#" + calUnit.getGmlID(), parentFeature, (IRelationType) property, wrappedFeature.getFeatureType() ); //$NON-NLS-1$
      parentFeature.setProperty( property, linkedFeature );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D.0" ), e ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getCalculationUnit()
   */
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
      final Feature resolveLink = FeatureHelper.resolveLink( this.getFeature(), ICalculationUnit1D2D.WB1D2D_PROP_CALC_UNIT );
      if( !(resolveLink instanceof XLinkedFeature_Impl) )
      {
        throw e;
      }
      final String featureId = ((XLinkedFeature_Impl) resolveLink).getFeatureId();
      final IFEDiscretisationModel1d2d model = Util.getModel( IFEDiscretisationModel1d2d.class );
      if( model == null )
      {
        throw e;
      }
      final GMLWorkspace modelWorkspace = model.getFeature().getWorkspace();
      final Feature calUnitFeature = modelWorkspace.getFeature( featureId );
      if( calUnitFeature == null )
      {
        return null;
      }
      final ICalculationUnit adapter = (ICalculationUnit) calUnitFeature.getAdapter( ICalculationUnit.class );
      return adapter;
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#get_p_bottom()
   */
  public Double get_P_BOTTOM( )
  {
    final Double property = (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_P_BOTTOM );
    return property != null ? property : 0.0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#get_RelaxationsFactor() changed to string
   *      to allow more flexible expansion of "Relaxation Factor"
   */
  public String get_RelaxationsFactor( )
  {
    return (String) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_RELAXATION_FACTOR );
  }

  // /**
  // * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#get_RelaxationsFactor()
  // */
  // public Double get_RelaxationsFactor( )
  // {
  // return (Double) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_RELAXATION_FACTOR );
  // }

  public boolean isSteadySelected( )
  {
    final Boolean property = (Boolean) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_STEADY_CHECKBOX );
    return property != null ? property.booleanValue() : false;
  }

  public boolean isUnsteadySelected( )
  {
    final Boolean property = (Boolean) getFeature().getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_UNSTEADY_CHECKBOX );
    return property != null ? property.booleanValue() : false;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#addRestartInfo()
   */
  public IRestartInfo addRestartInfo( )
  {
    return m_restartInfos.addNew( IRestartInfo.QNAME, IRestartInfo.class );
  }
}
