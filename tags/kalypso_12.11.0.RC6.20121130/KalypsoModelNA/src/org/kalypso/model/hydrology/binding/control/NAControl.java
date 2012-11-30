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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gml.ui.gml.IDuplicateFeatureMarker;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IXLinkedFeature;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Binding class for {org.kalypso.na.control_v2}NAControl.
 *
 * @author Gernot Belger
 */
public class NAControl extends Feature_Impl implements IDuplicateFeatureMarker
{
  private static final String NS_CONTROL = NaModelConstants.NS_NAMETA;

  public static final QName FEATURE_NACONTROL = new QName( NS_CONTROL, "NAControl" ); //$NON-NLS-1$

  private static final QName PROP_STARTSIMULATION = new QName( NS_CONTROL, "startsimulation" ); //$NON-NLS-1$

  private static final QName PROP_ENDSIMULATION = new QName( NS_CONTROL, "endsimulation" ); //$NON-NLS-1$

  private static final QName PROP_MINUTES_TIMESTEP = new QName( NS_CONTROL, "minutesTimestep" ); //$NON-NLS-1$

  private static final QName PROP_VERSION_KALYPSONA = new QName( NS_CONTROL, "versionKalypsoNA" ); //$NON-NLS-1$

  private static final QName PROP_PNS = new QName( NS_CONTROL, "pns" ); //$NON-NLS-1$

  public static final QName PROP_XJAH = new QName( NS_CONTROL, "xjah" ); //$NON-NLS-1$

  private static final QName PROP_XWAHL2 = new QName( NS_CONTROL, "xwahl2" ); //$NON-NLS-1$

  private static final QName PROP_IPVER = new QName( NS_CONTROL, "ipver" ); //$NON-NLS-1$

  private static final QName PROP_RETURN_PERIOD = new QName( NS_CONTROL, "returnPeriod" ); //$NON-NLS-1$

  private static final QName PROP_EDITOR = new QName( NS_CONTROL, "editor" ); //$NON-NLS-1$

  private static final QName PROP_COMMENT = new QName( NS_CONTROL, "comment" ); //$NON-NLS-1$

  private static final QName PROP_CREATION_TIME = new QName( NS_CONTROL, "creationTime" ); //$NON-NLS-1$

  public static final QName PROP_GENERATOR_N = new QName( NS_CONTROL, "generatorN" ); //$NON-NLS-1$

  public static final QName PROP_GENERATOR_T = new QName( NS_CONTROL, "generatorT" ); //$NON-NLS-1$

  public static final QName PROP_GENERATOR_E = new QName( NS_CONTROL, "generatorE" ); //$NON-NLS-1$

  public static final QName PROPERTY_MAPPING_GAUGE = new QName( NS_CONTROL, "mappingGaugeMeasurement" ); //$NON-NLS-1$

  public static final QName PROPERTY_MAPPING_NODE_INFLOW = new QName( NS_CONTROL, "mappingNodeInflow" ); //$NON-NLS-1$

  public static final QName PROPERTY_MAPPING_STORAGE_EVAPORATION = new QName( NS_CONTROL, "mappingStorageEvaporation" ); //$NON-NLS-1$

  public static final QName PROP_INITIAL_VALUE_SOURCE = new QName( NS_CONTROL, "initialValueSource" ); //$NON-NLS-1$

  private static final QName PROPERTY_LAST_MODIFIED = new QName( NS_CONTROL, "lastModified" ); //$NON-NLS-1$

  public NAControl( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    /* Initialize with some default values */
    setCreationTime( new Date() );
    setEditor( SystemUtils.USER_NAME );
    adjustDescription();
  }

  private void adjustDescription( )
  {
    /* Get the previous description. */
    final String previousDescription = getDescription();
    if( !StringUtils.isBlank( previousDescription ) )
      return;

    /* Get all simulations. */
    final SimulationCollection owner = (SimulationCollection) getOwner();
    if( owner == null )
      return;

    final IFeatureBindingCollection<NAControl> allSimulations = owner.getSimulations();

    /* Find the new description. */
    final String newDescription = findUniqueValue( allSimulations.toArray( new NAControl[] {} ), NAControl.QN_DESCRIPTION, Messages.getString( "NAControl.0" ), null ); //$NON-NLS-1$

    /* Set the new description. */
    setDescription( newDescription );
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

  public void setGeneratorReferenceN( final String href )
  {
    setGeneratorReference( PROP_GENERATOR_N, href );
  }

  public IRainfallGenerator getGeneratorT( )
  {
    return getGenerator( PROP_GENERATOR_T );
  }

  public void setGeneratorReferenceT( final String href )
  {
    setGeneratorReference( PROP_GENERATOR_T, href );
  }

  public IRainfallGenerator getGeneratorE( )
  {
    return getGenerator( PROP_GENERATOR_E );
  }

  public void setGeneratorReferenceE( final String href )
  {
    setGeneratorReference( PROP_GENERATOR_E, href );
  }

  private IRainfallGenerator getGenerator( final QName prop )
  {
    final IXLinkedFeature xlink = (IXLinkedFeature) getProperty( prop );
    if( xlink == null )
      return null;

    return (IRainfallGenerator) xlink.getFeature();
  }

  private void setGeneratorReference( final QName property, final String href )
  {
    setLink( property, href );
  }

  public String getInitialValueSource( )
  {
    return (String) getProperty( PROP_INITIAL_VALUE_SOURCE );
  }

  /**
   * This function returns the last modified timestamp.
   *
   * @return The last modified timestamp.
   */
  public Date getLastModified( )
  {
    final XMLGregorianCalendar lastModified = getProperty( PROPERTY_LAST_MODIFIED, XMLGregorianCalendar.class );

    return DateUtilities.toDate( lastModified );
  }

  /**
   * This function sets the last modified timestamp.
   *
   * @param lastModified
   *          The last modified timestamp.
   */
  public void setLastModified( final Date lastModified )
  {
    setProperty( PROPERTY_LAST_MODIFIED, DateUtilities.toXMLGregorianCalendar( lastModified ) );
  }

  /**
   * This function returns the last modified timestamp for all last modified values available.<br/>
   * {@link #getLastModified()}<br/>
   * {@link #getLastModifiedGenerators()}<br/>
   * {@link #getLastModifiedInputData()}
   *
   * @return The last modified timestamp.
   */
  public long getLastModifiedInput( )
  {
    /* This is the last modified timestamp of the this simulation itself. */
    long lastModifiedSimulation = -1;
    final Date lastModified = getLastModified();
    if( lastModified != null )
      lastModifiedSimulation = lastModified.getTime();

    /* This is the last modified timestamp of the catchment models. */
    final long lastModifiedGenerators = getLastModifiedGenerators();

    /* This is the last modified timestamp of the input data. */
    final long lastModifiedInputData = getLastModifiedInputData();

    return NumberUtils.max( new long[] { lastModifiedSimulation, lastModifiedGenerators, lastModifiedInputData } );
  }

  public long getLastModifiedGenerators( )
  {
    long result = -1;

    final IRainfallGenerator[] generators = new IRainfallGenerator[] { getGeneratorN(), getGeneratorT(), getGeneratorE() };
    for( final IRainfallGenerator generator : generators )
    {
      if( generator instanceof ILinearSumGenerator )
        result = Math.max( result, ((ILinearSumGenerator) generator).getLastModifiedInput() );
      else if( generator instanceof IMultiGenerator )
        result = Math.max( result, ((IMultiGenerator) generator).getLastModifiedInput() );
      else
      {
        if( generator != null )
        {
          final Date lastModified = generator.getLastModified();
          if( lastModified != null )
            result = Math.max( result, lastModified.getTime() );
        }
      }
    }

    return result;
  }

  public long getLastModifiedInputData( )
  {
    try
    {
      /* Get the .models folder of the current scenario. */
      final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();

      final RrmScenario scenario = new RrmScenario( scenarioFolder );

      /* This is the last modified timestamp of the modell.gml. */
      final IFile modellFile = scenario.getModelFile();
      final long modellLastModified = modellFile.getLocation().toFile().lastModified();

      /* This is the last modified timestamp of the parameter.gml. */
      final IFile parameterFile = scenario.getParameterGml();
      final long parameterLastModified = parameterFile.getLocation().toFile().lastModified();

      /* This is the last modified timestamp of the hydrotop.gml. */
      final IFile hydrotopFile = scenario.getHydrotopGml();
      final long hydrotopLastModified = hydrotopFile.getLocation().toFile().lastModified();

      return NumberUtils.max( new long[] { modellLastModified, parameterLastModified, hydrotopLastModified } );
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
      return -1;
    }
  }

  public void setMappingReferenceGauge( final String href )
  {
    setLink( PROPERTY_MAPPING_GAUGE, href );
  }

  public void setMappingReferenceNodeInflow( final String href )
  {
    setLink( PROPERTY_MAPPING_NODE_INFLOW, href );
  }

  public void setMappingReferenceStorageEvaporation( final String href )
  {
    setLink( PROPERTY_MAPPING_STORAGE_EVAPORATION, href );
  }

  public ITimeseriesMapping getMappingGauge( )
  {
    return (ITimeseriesMapping) resolveMember( PROPERTY_MAPPING_GAUGE );
  }

  public ITimeseriesMapping getMappingNodeInflow( )
  {
    return (ITimeseriesMapping) resolveMember( PROPERTY_MAPPING_NODE_INFLOW );
  }

  public ITimeseriesMapping getMappingStorageEvaporation( )
  {
    return (ITimeseriesMapping) resolveMember( PROPERTY_MAPPING_STORAGE_EVAPORATION );
  }

  @Override
  public void postDuplicated( final CommandableWorkspace workspace ) throws Exception
  {
    /* Get the previous description. */
    final String previousDescription = getDescription();

    /* Get all simulations. */
    final SimulationCollection owner = (SimulationCollection) getOwner();
    final IFeatureBindingCollection<NAControl> allSimulations = owner.getSimulations();

    /* Find the new description. */
    final String newDescription = findUniqueValue( allSimulations.toArray( new NAControl[] {} ), NAControl.QN_DESCRIPTION, previousDescription, Messages.getString( "NAControl.1" ) ); //$NON-NLS-1$

    /* Set the new description. */
    final IPropertyType pt = getFeatureType().getProperty( NAControl.QN_DESCRIPTION );
    workspace.postCommand( new ChangeFeatureCommand( this, pt, newDescription ) );
  }

  /**
   * This function checks all existing values of property and returns the ajusted value, if it does already exisit among
   * them.
   *
   * @param features
   *          The features to check.
   * @param property
   *          The property to check. Must be a string property.
   * @param value
   *          The intented value.
   * @param detail
   *          A detail string. It will be attached to the value, e.g. 'value (detail)'.
   * @return A unique value.
   */
  private String findUniqueValue( final Feature[] features, final QName property, final String value, final String detail )
  {
    /* All existing values. */
    final List<String> existingValues = new ArrayList<>();

    /* Collect existing values. */
    for( final Feature feature : features )
      existingValues.add( (String) feature.getProperty( property ) );

    /* The new value. */
    String newValue = detail != null ? String.format( "%s (%s)", value, detail ) : String.format( "%s", value ); //$NON-NLS-1$ //$NON-NLS-2$

    /* Find the new value. */
    int cnt = 1;
    while( true )
    {
      /* Check if it is already used. */
      if( !existingValues.contains( newValue ) )
        return newValue;

      /* The new value. */
      newValue = detail != null ? String.format( "%s (%s %d)", value, detail, cnt++ ) : String.format( "%s %d", value, cnt++ ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}