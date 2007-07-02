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
import java.net.URI;
import java.net.URL;
import java.util.Calendar;
import java.util.GregorianCalendar;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.FileLocator;
import org.kalypso.commons.resources.FileUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Patrice Congo
 * @author Madanagopal
 * 
 */
@SuppressWarnings("unchecked")
public class ControlModel1D2D extends AbstractFeatureBinder implements IControlModel1D2D
{
  public ControlModel1D2D( final Feature featureToBind )
  {
    this( featureToBind, Kalypso1D2DSchemaConstants.WB1D2DCONTROL_F_MODEL );
  }

  public ControlModel1D2D( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getTimeSteps()
   */
  public IObservation<TupleResult> getTimeSteps( )
  {
    try
    {
      final Feature feature = getFeature();
      final Object property = feature.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
      final IObservation<TupleResult> obs = (IObservation<TupleResult>) ((Feature) property).getAdapter( IObservation.class );
      return obs;
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }

  public Integer getIDNOPT( )
  {
    return (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IDNOPT );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getVersion()
   */
  public String getVersion( )
  {
    return (String) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_VERSION );
  }

  public int getStartYear( )
  {
    return getStartCalendar().getYear();
  }

  public Integer getIaccyc( )
  {
    return (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IACCYC );
  }

  public boolean getRestart( )
  {
    return ((Boolean) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_RESTART )).booleanValue();
  }

  public XMLGregorianCalendar getStartCalendar( )
  {
    return (XMLGregorianCalendar) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_STARTSIM );
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
    return (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IEDSW );
  }

  public Double getTBFACT( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TBFACT );
  }

  public Double getTBMIN( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TBMIN );
  }

  public Double getOMEGA( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_OMEGA );
  }

  public Double getELEV( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_ELEV );
  }

  public Double getUDIR( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_UDIR );
  }

  public Double getUNOM( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_UNOM );
  }

  public Double getHMIN( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_HMIN );
  }

  public Double getDSET( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DSET );
  }

  public Double getDSETD( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DSETD );
  }

  public Integer getNITI( )
  {
    Integer property = (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_NITI );
    if(property == null) property = 0;
    return property;
  }

  public Integer getNITN( )
  {
    Integer property = (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_NITN );
    if(property == null) property = 0;
    return property;
  }

  public Integer getNCYC( )
  {
    return (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_NCYC );
  }

  public Double getCONV_1( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_1 );
  }

  public Double getCONV_2( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_2 );
  }

  public Double getCONV_3( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_3 );
  }

  public Integer getIDRPT( )
  {
    return (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IDRPT );
  }

  public Double getDRFACT( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DRFACT );
  }

  public boolean getVegeta( )
  {
    return (Boolean) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_VEGETA );
  }

  public Double getAC1( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC1 );
  }

  public Double getAC2( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC2 );
  }

  public Double getAC3( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC3 );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#setCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  public void setCalculationUnit( ICalculationUnit calUnit )
  {
    try
    {
      Feature wrappedFeature = calUnit.getWrappedFeature();
      Feature parentFeature = getFeature();//control to link to 
      IPropertyType property = parentFeature.getFeatureType().getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_CALC_UNIT );
      
      GMLWorkspace workspace = calUnit.getWrappedFeature().getWorkspace();
      URL context = workspace.getContext();      
      File filee = new File(FileLocator.resolve( context ).getFile() );
      String name = filee.getName();//new java.io.File( uri ).getName();
      XLinkedFeature_Impl linkedFeature =
        (XLinkedFeature_Impl) FeatureHelper.createLinkToID( name+"#"+calUnit.getGmlID(),parentFeature, (IRelationType) property, wrappedFeature.getFeatureType() );
      parentFeature.setProperty( property, linkedFeature );
    }
    catch (Exception e) {
      e.printStackTrace();
      throw new RuntimeException( "Exception while setting calunit link to control", e );
    }
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getCalculationUnit()
   */
  public ICalculationUnit getCalculationUnit( )
  {
    ICalculationUnit resolveLink = FeatureHelper.resolveLink( this, Kalypso1D2DSchemaConstants.WB1D2D_PROP_CALC_UNIT, ICalculationUnit.class );
    return resolveLink;
  }
  
}
