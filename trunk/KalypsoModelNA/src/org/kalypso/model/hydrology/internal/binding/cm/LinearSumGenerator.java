/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.hydrology.internal.binding.cm;

import java.io.File;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.LocalTime;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.model.hydrology.util.cm.CatchmentHelper;
import org.kalypso.model.rcm.binding.AbstractRainfallGenerator;
import org.kalypso.model.rcm.util.RainfallGeneratorUtilities;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.util.TimestampHelper;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.core.filter.binding.IZmlFilter;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * The linear sum generator.
 *
 * @author Holger Albert
 */
public class LinearSumGenerator extends AbstractRainfallGenerator implements ILinearSumGenerator
{
  /**
   * The catchments.
   */
  private final IFeatureBindingCollection<ICatchment> m_catchments;

  /**
   * The constructor.
   *
   * @param parent
   *          The parent.
   * @param parentRelation
   *          The parent relation.
   * @param ft
   *          The feature type.
   * @param id
   *          The id.
   * @param propValues
   *          The property values.
   */
  public LinearSumGenerator( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );

    m_catchments = new FeatureBindingCollection<ICatchment>( this, ICatchment.class, MEMBER_CATCHMENT );
  }

  /**
   * This function calculates the catchment model.
   *
   * @param catchmentFeatures
   *          The catchment features will be taken from the generator itself, so they are not needed here. Leave them
   *          <code>null</code>.
   * @param range
   *          The date range.
   * @param log
   *          The log.
   * @param monitor
   *          A progress monitor.
   */
  @Override
  public IObservation[] createRainfall( final Feature[] catchmentFeatures, final DateRange range, final ILog log, IProgressMonitor monitor ) throws CoreException
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Get the catchments. */
      final List<ICatchment> catchments = getCatchments();

      /* HINT: Keep in mind, that the results must match the order of the catchments array. */
      final IObservation[] results = new IObservation[catchments.size()];

      /* Monitor. */
      monitor.beginTask( String.format( "Generiere Zeitreihen f�r %d Gebiete...", catchments.size() ), catchments.size() * 200 );
      monitor.subTask( "Generiere Zeitreihen..." );

      /* A catchment with the already used hash needs not to be calculated anymore. */
      /* Because the result timeseries would be the same. */
      /* Also the link in the catchment to the result timeseries should be the same. */
      final List<String> usedHashes = new ArrayList<String>();

      /* Generate one timeseries for each catchment. */
      for( int i = 0; i < catchments.size(); i++ )
      {
        /* Get the catchment. */
        final ICatchment catchment = catchments.get( i );
        final String description = catchment.getDescription();

        /* Generate the message 1. */
        final String message1 = String.format( "Sammle gewichtete Zeitreihen zur Erzeugung von Zeitreihe %d zu Gebiet '%s'...", i + 1, description );

        /* Monitor. */
        monitor.subTask( message1 );

        /* Log. */
        if( log != null )
          log.log( new Status( IStatus.INFO, ModelNA.PLUGIN_ID, message1 ) );

        /* Memory for the factors and the observations of the catchments. */
        final List<Double> factors = new ArrayList<Double>();
        final List<IObservation> observations = new ArrayList<IObservation>();

        /* Build the hash. */
        final String hash = CatchmentHelper.buildHash( catchment );

        /* Was the target timeseries already generated by a previous catchment with the same hash. */
        if( !usedHashes.contains( hash ) )
        {
          /* Get the factorized timeseries. */
          for( final IFactorizedTimeseries factorizedTimeseries : catchment.getFactorizedTimeseries() )
          {
            /* Get the factor. */
            final BigDecimal factor = factorizedTimeseries.getFactor();

            /* Get the timeseries link. */
            final GMLXPath linkPath = new GMLXPath( IFactorizedTimeseries.PROPERTY_TIMESERIES_LINK );

            /* Get the filters from the gml. */
            final IZmlFilter[] filters = getFilters().toArray( new IZmlFilter[] {} );

            /* Load the observation. */
            final IObservation[] readObservations = RainfallGeneratorUtilities.readObservations( new Feature[] { factorizedTimeseries }, linkPath, filters, range );
            final IObservation observation = readObservations[0];

            /* If the factor is valid, add the factor and its observation. */
            if( factor != null && factor.intValue() > 0 && factor.intValue() <= 100 )
            {
              factors.add( new Double( factor.doubleValue() / 100 ) );
              observations.add( observation );
            }
          }

          /* Store the hash. */
          usedHashes.add( hash );
        }

        /* Generate the message 2. */
        final String message2 = String.format( "Erzeuge Zeitreihe %d zu Gebiet '%s' mit %d gewichteten Zeitreihen...", i + 1, description, factors.size() );

        /* Monitor. */
        monitor.worked( 100 );
        monitor.subTask( message2 );

        /* Log. */
        if( log != null )
          log.log( new Status( IStatus.INFO, ModelNA.PLUGIN_ID, message2 ) );

        /* Set the resulting observation for this catchment. */
        final double[] factorDoubles = ArrayUtils.toPrimitive( factors.toArray( new Double[] {} ) );
        results[i] = RainfallGeneratorUtilities.combineObses( observations.toArray( new IObservation[] {} ), factorDoubles, "catchmentGenerator://linearSum" );

        /* Monitor. */
        monitor.worked( 100 );
      }

      return results;
    }
    catch( final Exception ex )
    {
      /* Create the error status. */
      final IStatus status = new Status( IStatus.ERROR, ModelNA.PLUGIN_ID, ex.getLocalizedMessage(), ex );

      /* If there is a log, log to it. */
      if( log != null )
        log.log( status );

      throw new CoreException( status );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.model.rcm.binding.AbstractRainfallGenerator#setValidFrom(java.util.Date)
   */
  @Override
  public void setValidFrom( final Date validFrom )
  {
    /* First set. */
    setProperty( PROPERTY_VALID_FROM, DateUtilities.toXMLGregorianCalendar( validFrom ) );

    /* Adjust the validities. */
    adjustValidities();
  }

  /**
   * @see org.kalypso.model.rcm.binding.AbstractRainfallGenerator#setValidTo(java.util.Date)
   */
  @Override
  public void setValidTo( final Date validTo )
  {
    /* First set. */
    setProperty( PROPERTY_VALID_TO, DateUtilities.toXMLGregorianCalendar( validTo ) );

    /* Adjust the validities. */
    adjustValidities();
  }

  @Override
  public String getComment( )
  {
    return getProperty( PROPERTY_COMMENT, String.class );
  }

  @Override
  public void setComment( final String comment )
  {
    setProperty( PROPERTY_COMMENT, comment );
  }

  @Override
  public Integer getTimestep( )
  {
    return getProperty( PROPERTY_TIMESTEP, Integer.class );
  }

  @Override
  public void setTimestep( final Integer timestep )
  {
    setProperty( PROPERTY_TIMESTEP, timestep );
  }

  /**
   * @see org.kalypso.model.rcm.binding.ILinearSumGenerator#getTimestamp()
   */
  @Override
  public LocalTime getTimestamp( )
  {
    /* Get the property. */
    final String timestampText = getProperty( PROPERTY_TIMESTAMP, String.class );
    if( StringUtils.isBlank( timestampText ) )
      return null;

    try
    {
      /* Parse the timestamp. */
      return TimestampHelper.parseTimestamp( timestampText );
    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.model.rcm.binding.ILinearSumGenerator#setTimestamp(org.joda.time.LocalTime)
   */
  @Override
  public void setTimestamp( final LocalTime timestamp )
  {
    /* REMARK: We assume the values are in UTC. */
    if( timestamp == null )
      setProperty( PROPERTY_TIMESTAMP, null );
    else
      setProperty( PROPERTY_TIMESTAMP, TimestampHelper.toTimestampText( timestamp ) );

    /* Adjust the validities. */
    adjustValidities();
  }

  @Override
  public GMLXPath getAreaNamePath( )
  {
    return getPath( PROPERTY_AREA_NAME );
  }

  @Override
  public void setAreaNamePath( final GMLXPath path )
  {
    setPath( PROPERTY_AREA_NAME, path );
  }

  @Override
  public GMLXPath getAreaDescriptionPath( )
  {
    return getPath( PROPERTY_AREA_DESCRIPTION );
  }

  @Override
  public void setAreaDescriptionPath( final GMLXPath path )
  {
    setPath( PROPERTY_AREA_DESCRIPTION, path );
  }

  @Override
  public GMLXPath getAreaPath( )
  {
    return getPath( PROPERTY_AREA );
  }

  @Override
  public void setAreaPath( final GMLXPath path )
  {
    setPath( PROPERTY_AREA, path );
  }

  private GMLXPath getPath( final QName property )
  {
    final String value = getProperty( property, String.class );
    if( StringUtils.isBlank( value ) )
      return null;

    return new GMLXPath( value, getWorkspace().getNamespaceContext() );
  }

  private void setPath( final QName property, final GMLXPath path )
  {
    if( path == null )
      setProperty( property, null );
    else
      setProperty( property, path.toString() );
  }

  @Override
  public IFeatureBindingCollection<ICatchment> getCatchments( )
  {
    return m_catchments;
  }

  /**
   * @see org.kalypso.model.rcm.binding.ILinearSumGenerator#adjustValidities()
   */
  @Override
  public void adjustValidities( )
  {
    /* Get the valid from date and the valid to date. */
    final Date validFrom = getValidFrom();
    final Date validTo = getValidTo();
    if( validFrom == null || validTo == null )
      return;

    /* Get the valid from date. */
    final Calendar validFromCalendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    validFromCalendar.setTime( validFrom );

    /* Get the valid to date. */
    final Calendar validToCalendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    validToCalendar.setTime( validTo );

    /* Get the timestamp. */
    final LocalTime timestamp = getTimestamp();
    if( timestamp != null )
    {
      /* Convert to a date with the kalypso timezone. */
      /* The date fields are ignored. */
      final DateTime timestampUTC = timestamp.toDateTimeToday( DateTimeZone.forTimeZone( TimeZone.getTimeZone( "UTC" ) ) );
      final DateTime timestampDate = new DateTime( timestampUTC.toDate(), DateTimeZone.forTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() ) );

      /* With a timestamp adjust the hours and the minutes accordingly. */
      validFromCalendar.set( Calendar.HOUR_OF_DAY, timestampDate.getHourOfDay() );
      validFromCalendar.set( Calendar.MINUTE, timestampDate.getMinuteOfHour() );
      validToCalendar.set( Calendar.HOUR_OF_DAY, timestampDate.getHourOfDay() );
      validToCalendar.set( Calendar.MINUTE, timestampDate.getMinuteOfHour() );
    }
    else
    {
      /* Without a timestemp set the hours and the minutes to zero. */
      validFromCalendar.set( Calendar.HOUR_OF_DAY, 0 );
      validFromCalendar.set( Calendar.MINUTE, 0 );
      validToCalendar.set( Calendar.HOUR_OF_DAY, 0 );
      validToCalendar.set( Calendar.MINUTE, 0 );
    }

    /* Always set the seconds and milliseconds to zero. */
    validFromCalendar.set( Calendar.SECOND, 0 );
    validFromCalendar.set( Calendar.MILLISECOND, 0 );
    validToCalendar.set( Calendar.SECOND, 0 );
    validToCalendar.set( Calendar.MILLISECOND, 0 );

    /* Set the properties. */
    setProperty( PROPERTY_VALID_FROM, DateUtilities.toXMLGregorianCalendar( validFromCalendar.getTime() ) );
    setProperty( PROPERTY_VALID_TO, DateUtilities.toXMLGregorianCalendar( validToCalendar.getTime() ) );
  }

  @Override
  public long getLastModifiedInput( )
  {
    /* This is the last modified timestamp of the this generator itself. */
    final long lastModified = getLastModified();

    /* This is the last modified timestamp of the timeseries. */
    final long lastModifiedTimeseries = getLastModifiedTimeseries();

    /* This is the last modified timestamp of the catchments. */
    final long lastModifiedCatchments = getLastModifiedCatchments();

    return NumberUtils.max( new long[] { lastModified, lastModifiedTimeseries, lastModifiedCatchments } );
  }

  @Override
  public long getLastModifiedTimeseries( )
  {
    final IFeatureBindingCollection<ICatchment> catchments = getCatchments();
    if( catchments == null || catchments.size() == 0 )
      return -1;

    long result = -1;
    for( final ICatchment catchment : catchments )
    {
      final IFeatureBindingCollection<IFactorizedTimeseries> timeseries = catchment.getFactorizedTimeseries();
      for( final IFactorizedTimeseries oneTimeseries : timeseries )
      {
        final ZmlLink timeseriesLink = oneTimeseries.getTimeseriesLink();
        final BigDecimal factor = oneTimeseries.getFactor();
        if( timeseriesLink == null || !timeseriesLink.isLinkExisting() || factor == null || factor.intValue() == 0 )
          continue;

        final File javaFile = timeseriesLink.getJavaFile();
        final long lastModified = javaFile.lastModified();
        result = Math.max( result, lastModified );
      }
    }

    return result;
  }

  @Override
  public long getLastModifiedCatchments( )
  {
    try
    {
      /* Get the .models folder of the current scenario. */
      final IScenarioDataProvider dataProvider = ScenarioHelper.getScenarioDataProvider();
      final IContainer scenarioFolder = dataProvider.getScenarioFolder();
      final IFolder modelsFolder = scenarioFolder.getFolder( new Path( INaProjectConstants.FOLDER_MODELS ) );

      /* This is the last modified timestamp of the modell.gml. */
      final IFile modellFile = modelsFolder.getFile( INaProjectConstants.GML_MODELL_FILE );

      return modellFile.getLocation().toFile().lastModified();
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
      return -1;
    }
  }
}