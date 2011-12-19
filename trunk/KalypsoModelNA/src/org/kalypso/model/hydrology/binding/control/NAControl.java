/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.hydrology.binding.control;

import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * Binding class for {org.kalypso.na.control_v2}NAControl.
 *
 * @author Gernot Belger
 */
public class NAControl extends Feature_Impl
{
  private static final String NS_CONTROL = NaModelConstants.NS_NAMETA;

  public static final QName FEATURE_NACONTROL = new QName( NS_CONTROL, "NAControl" ); //$NON-NLS-1$

  private static final QName PROP_STARTSIMULATION = new QName( NS_CONTROL, "startsimulation" ); //$NON-NLS-1$

  private static final QName PROP_ENDSIMULATION = new QName( NS_CONTROL, "endsimulation" ); //$NON-NLS-1$

  private static final QName PROP_MINUTES_TIMESTEP = new QName( NS_CONTROL, "minutesTimestep" ); //$NON-NLS-1$

  private static final QName PROP_VERSION_KALYPSONA = new QName( NS_CONTROL, "versionKalypsoNA" ); //$NON-NLS-1$

  private static final QName PROP_PNS = new QName( NS_CONTROL, "pns" ); //$NON-NLS-1$

  private static final QName PROP_XJAH = new QName( NS_CONTROL, "xjah" ); //$NON-NLS-1$

  private static final QName PROP_XWAHL2 = new QName( NS_CONTROL, "xwahl2" ); //$NON-NLS-1$

  private static final QName PROP_IPVER = new QName( NS_CONTROL, "ipver" ); //$NON-NLS-1$

  private static final QName PROP_RETURN_PERIOD = new QName( NS_CONTROL, "returnPeriod" ); //$NON-NLS-1$

  private static final QName PROP_EDITOR = new QName( NS_CONTROL, "editor" ); //$NON-NLS-1$

  private static final QName PROP_COMMENT = new QName( NS_CONTROL, "comment" ); //$NON-NLS-1$

  private static final QName PROP_CREATION_TIME = new QName( NS_CONTROL, "creationTime" ); //$NON-NLS-1$

  private static final QName PROP_GENERATOR_N = new QName( NS_CONTROL, "generatorN" ); //$NON-NLS-1$

  private static final QName PROP_GENERATOR_T = new QName( NS_CONTROL, "generatorT" ); //$NON-NLS-1$

  private static final QName PROP_GENERATOR_E = new QName( NS_CONTROL, "generatorE" ); //$NON-NLS-1$

  public NAControl( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public Date getSimulationStart( )
  {
    return DateUtilities.toDate( getProperty( PROP_STARTSIMULATION, XMLGregorianCalendar.class ) );
  }

  public void setSimulationStart( final Date simulationStart )
  {
    setProperty( PROP_STARTSIMULATION, DateUtilities.toXMLGregorianCalendar( simulationStart ) );
  }

  public Date getSimulationEnd( )
  {
    return DateUtilities.toDate( getProperty( PROP_ENDSIMULATION, XMLGregorianCalendar.class ) );
  }

  public void setSimulationEnd( final Date simulationEnd )
  {
    setProperty( PROP_ENDSIMULATION, DateUtilities.toXMLGregorianCalendar( simulationEnd ) );
  }

  public Integer getMinutesOfTimestep( )
  {
    final Integer minutesOfTimeStep = getProperty( PROP_MINUTES_TIMESTEP, Integer.class );

    if( minutesOfTimeStep == null || minutesOfTimeStep.intValue() == 0 )
      return 60;

    return minutesOfTimeStep.intValue();
  }

  public void setMinutesOfTimestep( final Integer minutesOfTimestep )
  {
    setProperty( PROP_MINUTES_TIMESTEP, minutesOfTimestep );
  }

  public boolean isUsePrecipitationForm( )
  {
    final Boolean value = getProperty( PROP_PNS, Boolean.class );
    if( value == null )
      return false;

    return value.booleanValue();
  }

  public void setUsePrecipitationForm( final boolean usePrecipitationForm )
  {
    setProperty( PROP_PNS, usePrecipitationForm );
  }

  public double getAnnuality( )
  {
    // the GUI asks for return period [a] - the fortran kernal needs annuality [1/a]
    final Double returnPeriod = getProperty( PROP_XJAH, Double.class );
    if( returnPeriod == null )
      return Double.NaN;

    return 1d / returnPeriod;
  }

  private Double getDurationMinutes( )
  {
    return (Double) getProperty( PROP_XWAHL2 );
  }

  public void setDurationMinutes( final Double durationMinutes )
  {
    setProperty( PROP_XWAHL2, durationMinutes );
  }

  public double getDurationHours( )
  {
    final Double durationMinutes = getDurationMinutes();
    if( durationMinutes == null )
      return Double.NaN;

    return durationMinutes / 60d;
  }

  public String getPrecipitationForm( )
  {
    return getProperty( PROP_IPVER, String.class );
  }

  public void setPrecipitationForm( final String precipitationForm )
  {
    setProperty( PROP_IPVER, precipitationForm );
  }

  public String getExeVersion( )
  {
    return getProperty( PROP_VERSION_KALYPSONA, String.class );
  }

  public void setExeVersion( final String version )
  {
    setProperty( PROP_VERSION_KALYPSONA, version );
  }

  public Integer getReturnPeriod( )
  {
    return getProperty( PROP_RETURN_PERIOD, Integer.class );
  }

  public void setReturnPeriod( final Integer returnPeriod )
  {
    setProperty( PROP_RETURN_PERIOD, returnPeriod );
  }

  public String getEditor( )
  {
    return getProperty( PROP_EDITOR, String.class );
  }

  public void setEditor( final String editor )
  {
    setProperty( PROP_EDITOR, editor );
  }

  public String getComment( )
  {
    return getProperty( PROP_COMMENT, String.class );
  }

  public void setComment( final String comment )
  {
    setProperty( PROP_COMMENT, comment );
  }

  public Date getCreationTime( )
  {
    final XMLGregorianCalendar calendar = getProperty( PROP_CREATION_TIME, XMLGregorianCalendar.class );
    return DateUtilities.toDate( calendar );
  }

  public void setCreationTime( final Date creationTime )
  {
    final XMLGregorianCalendar calendar = DateUtilities.toXMLGregorianCalendar( creationTime );
    setProperty( PROP_CREATION_TIME, calendar );
  }

  public IRainfallGenerator getGeneratorN( )
  {
    return getGenerator( PROP_GENERATOR_N );
  }

  public IRainfallGenerator getGeneratorT( )
  {
    return getGenerator( PROP_GENERATOR_T );
  }

  public IRainfallGenerator getGeneratorE( )
  {
    return getGenerator( PROP_GENERATOR_E );
  }

  private IRainfallGenerator getGenerator( final QName prop )
  {
    final XLinkedFeature_Impl xlink = (XLinkedFeature_Impl) getProperty( prop );
    if( xlink == null )
      return null;

    return (IRainfallGenerator) xlink.getFeature();
  }
}