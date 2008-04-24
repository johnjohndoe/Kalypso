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
package org.kalypso.ogc.sensor.view.wizard.cvssheet;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleAxis;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableFactory;
import org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTableSet;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.CSV_COLUMN_SEPERATORS;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.DECIMAL_NUMBER_SEPERATORS;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.TSM_KEY;
import org.kalypso.ogc.sensor.view.wizard.cvssheet.CsvSheetImportDataModel.WQ_KIND;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.repository.file.FileItem;

import au.com.bytecode.opencsv.CSVReader;

/**
 * Perform finish worker for cvs import wizard.
 * 
 * @author kuch
 */
public class CsvSheetImportWorker implements ICoreRunnableWithProgress
{
  protected final CsvSheetImportDataModel m_model;

  public CsvSheetImportWorker( final CsvSheetImportDataModel model )
  {
    m_model = model;
  }

  private SimpleObservation addTimeSeries( final CSVReader readerTimeSeries ) throws IOException, ParseException, SensorException
  {
    final SimpleDateFormat dateFormat = (SimpleDateFormat) m_model.getValue( TSM_KEY.eCsvDateFormat );
// dateFormat.set2DigitYearStart( Calendar.getInstance().getTime() );

    final WQ_KIND kind = (WQ_KIND) m_model.getValue( TSM_KEY.eCsvTimeSeriesIsWorQ );
    final DECIMAL_NUMBER_SEPERATORS dSep = (DECIMAL_NUMBER_SEPERATORS) m_model.getValue( TSM_KEY.eCsvDecimalNumberSeperator );

    /* axis date */
    final IAxis defaultDate = TimeserieUtils.createDefaulAxis( TimeserieConstants.TYPE_DATE, true );
    final SimpleAxis simpleDateAxis = new SimpleAxis( defaultDate );
    simpleDateAxis.setName( "date" );

    /* axis values */
    final IAxis axisX;
    if( WQ_KIND.eW.equals( kind ) )
      axisX = TimeserieUtils.createDefaulAxis( TimeserieConstants.TYPE_WATERLEVEL, false );
    else if( WQ_KIND.eQ.equals( kind ) )
      axisX = TimeserieUtils.createDefaulAxis( TimeserieConstants.TYPE_RUNOFF, false );
    else
      throw new NotImplementedException();

    final SimpleAxis simpleDataAxis = new SimpleAxis( axisX );

    final SimpleTuppleModel simpleModel = new SimpleTuppleModel( new IAxis[] { simpleDateAxis, simpleDataAxis } );

    String[] nextLine;
    while( (nextLine = readerTimeSeries.readNext()) != null )
    {
      final String rDate = nextLine[0];
      final String rValue = nextLine[1];

      simpleModel.addTupple( new Object[] { dateFormat.parse( rDate ), new Double( rValue.replaceAll( dSep.getRegEx(), "." ) ) } );
    }

    final SimpleObservation simple = new SimpleObservation( new IAxis[] { simpleDateAxis, simpleDataAxis } );
    simple.setValues( simpleModel );

    return simple;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException
  {
    try
    {
      return importCsvSheet();
    }
    catch( final FileNotFoundException e )
    {
      return new Status( IStatus.ERROR, "CsvSheetImportWorker", e.getMessage() );
    }
    catch( final IOException e )
    {
      return new Status( IStatus.ERROR, "CsvSheetImportWorker", e.getMessage() );
    }
    catch( final ParseException e )
    {
      return new Status( IStatus.ERROR, "CsvSheetImportWorker", e.getMessage() );
    }
    catch( final WQException e )
    {
      return new Status( IStatus.ERROR, "CsvSheetImportWorker", e.getMessage() );
    }
    catch( final SensorException e )
    {
      return new Status( IStatus.ERROR, "CsvSheetImportWorker", e.getMessage() );
    }
  }

  private WQTableSet getWQTableSet( final CSVReader readerWQ ) throws NumberFormatException, IOException
  {
    final DECIMAL_NUMBER_SEPERATORS dSep = (DECIMAL_NUMBER_SEPERATORS) m_model.getValue( TSM_KEY.eCsvDecimalNumberSeperator );

    final List<Double> colOne = new ArrayList<Double>();
    final List<Double> colTwo = new ArrayList<Double>();

    String[] nextLine;
    while( (nextLine = readerWQ.readNext()) != null )
    {
      final Double dOne = new Double( nextLine[0].replaceAll( dSep.getRegEx(), "." ) );
      final Double dTwo = new Double( nextLine[1].replaceAll( dSep.getRegEx(), "." ) );

      colOne.add( dOne );
      colTwo.add( dTwo );
    }

    if( colOne.size() != colTwo.size() )
      throw new IllegalStateException();

    final WQ_KIND fistColum = (WQ_KIND) m_model.getValue( TSM_KEY.eCsvWqConnectionFirstColumn );
    if( WQ_KIND.eW.equals( fistColum ) )
    {
      final WQTable table = new WQTable( new Date(), colOne.toArray( new Double[] {} ), colTwo.toArray( new Double[] {} ) );

      return new WQTableSet( new WQTable[] { table }, "W", "Q" );
    }
    else if( WQ_KIND.eQ.equals( fistColum ) )
    {
      final WQTable table = new WQTable( new Date(), colTwo.toArray( new Double[] {} ), colOne.toArray( new Double[] {} ) );

      return new WQTableSet( new WQTable[] { table }, "W", "Q" );
    }

    throw new NotImplementedException();

  }

  private IStatus importCsvSheet( ) throws IOException, ParseException, WQException, SensorException
  {
    m_model.getValue( TSM_KEY.eCsvTimeSeriesFile );

    final CSV_COLUMN_SEPERATORS cSep = (CSV_COLUMN_SEPERATORS) m_model.getValue( TSM_KEY.eCsvColumnSeperator );

    /*******************************************************************************************************************
     * PROCESS TIME SERIES
     ******************************************************************************************************************/
    final File fTimeSeries = (File) m_model.getValue( TSM_KEY.eCsvTimeSeriesFile );
    final CSVReader readerTimeSeries = new CSVReader( new FileReader( fTimeSeries ), cSep.getValue() );

    final SimpleObservation simple = addTimeSeries( readerTimeSeries );

    /* meta data list */
    final org.kalypso.ogc.sensor.MetadataList mdl = simple.getMetadataList();

    /*******************************************************************************************************************
     * PROCESS WQ CONNECTION
     ******************************************************************************************************************/
    final File fWQConnection = (File) m_model.getValue( TSM_KEY.eCsvTimeSeriesWqConnectionFile );

    if( fWQConnection != null )
    {
      final CSVReader readerWQ = new CSVReader( new FileReader( fWQConnection ), cSep.getValue() );

      final WQTableSet wqSet = getWQTableSet( readerWQ );
      final String xmlString = WQTableFactory.createXMLString( wqSet );

      if( xmlString != null )
        mdl.setProperty( "WQ-Tabelle", xmlString );
    }

    /*******************************************************************************************************************
     * PROCESS META DATA
     ******************************************************************************************************************/
    /* name */
    final String name = (String) m_model.getValue( TSM_KEY.eCsvName );
    if( name != null )
    {
      mdl.setProperty( "name", name );
      simple.setName( name );
    }

    /* description */
    final String description = (String) m_model.getValue( TSM_KEY.eCsvDescription );
    if( description != null )
      mdl.setProperty( "description", description );

    /* river */
    final String river = (String) m_model.getValue( TSM_KEY.eCsvRiver );
    if( river != null )
      mdl.setProperty( "river", river );

    /* position */
    final String position = (String) m_model.getValue( TSM_KEY.eCsvPosition );
    if( position != null )
      mdl.setProperty( "position", position );

    /* create file at destination dir */
    final File destination = (File) m_model.getValue( TSM_KEY.eDestinationDir );
    final File senke = new File( destination, name + ".zml" );
    ZmlFactory.writeToFile( simple, senke );

    new UIJob( "updating observation tree" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {
        final FileItem item = (FileItem) m_model.getValue( TSM_KEY.eFileItem );
        item.getRepository().fireRepositoryStructureChanged();

        return Status.OK_STATUS;
      }
    }.schedule();

    return Status.OK_STATUS;
  }
}
