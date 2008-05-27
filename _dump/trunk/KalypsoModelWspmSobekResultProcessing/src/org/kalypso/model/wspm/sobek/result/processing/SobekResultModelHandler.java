package org.kalypso.model.wspm.sobek.result.processing;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.XMLGregorianCalendar;

import nl.wldelft.fews.pi.EventComplexType;
import nl.wldelft.fews.pi.TimeSerieComplexType;
import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.ResultTimeSeriesHandler;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class SobekResultModelHandler implements ISobekResultModel
{
  private final IFolder m_resultFolder;

  private static final Map<ICrossSectionNode, GMLWorkspace> m_workspaces = new HashMap<ICrossSectionNode, GMLWorkspace>();

  private static final Map<ICrossSectionNode, CommandableWorkspace> m_commandables = new HashMap<ICrossSectionNode, CommandableWorkspace>();

  private SobekResultModelHandler( final IFolder resultFolder )
  {
    m_resultFolder = resultFolder;
  }

  /**
   * @param resultFolder
   *            Sobek result folder
   * @return Sobek Result Model
   */
  public static ISobekResultModel getHandler( final IFolder resultFolder ) throws CoreException
  {
    if( !resultFolder.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( "Sobek result folder doesn't exists" ) );

    return new SobekResultModelHandler( resultFolder );
  }

  private IFile getCrossSectionNodeResultFile( final ICrossSectionNode node, final String extension ) throws CoreException
  {

    final IFolder folder = m_resultFolder.getFolder( "output/nodes/csn/" );
    if( !folder.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( "Sobek cross section node result folder doesn't exists" ) );

    final IFile iFile = folder.getFile( node.getId() + extension );
    if( !iFile.exists() )
      return null;

    return iFile;
  }

  public TimeSeriesComplexType getCrossSectionBinding( final ICrossSectionNode node ) throws CoreException
  {
    final IFile iFile = getCrossSectionNodeResultFile( node, ".xml" );

    final InputStream is = new BufferedInputStream( iFile.getContents() );
    try
    {
      final JAXBContext jc = JAXBContext.newInstance( nl.wldelft.fews.pi.ObjectFactory.class );
      final Unmarshaller u = jc.createUnmarshaller();
      final JAXBElement<TimeSeriesComplexType> jaxRoot = (JAXBElement<TimeSeriesComplexType>) u.unmarshal( is );

      return jaxRoot.getValue();
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( "Reading Sobek Cross Section Node data file - failed." ) );
    }
    finally
    {
      try
      {
        is.close();
      }
      catch( final IOException e )
      {
        throw new CoreException( StatusUtilities.createWarningStatus( "Error closing Sobek Cross Section Node input stream." ) );
      }
    }
  }

  public GMLWorkspace getCrossSectionTimeSeries( final ICrossSectionNode node ) throws CoreException
  {
    try
    {
      final CommandableWorkspace cmd = m_commandables.get( node );
      if( cmd != null )
        return cmd;

      /* get cross section node result file */
      IFile iFile = getCrossSectionNodeResultFile( node, ".gml" );

      boolean empty = false;

      /* gml file doesn't exists - create a new empty gml (workspace) file */
      if( iFile == null )
      {
        final IFile iCSN = getCrossSectionNodeResultFile( node, ".xml" );

        final IFolder parent = (IFolder) iCSN.getParent();
        iFile = parent.getFile( node.getId() + ".gml" );

        final InputStream stream = this.getClass().getResourceAsStream( "templates/templateSobekResultTimeSeries.gml" );
        iFile.create( stream, true, new NullProgressMonitor() );

        empty = true;
      }

      /* parse gml workspace */
      final URL url = iFile.getRawLocationURI().toURL();

      final GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( url, null );
      final CommandableWorkspace workspace = new CommandableWorkspace( gmlWorkspace );

      registerWorkspaces( node, gmlWorkspace, workspace );

      /* fill empty workspace with results */
      if( empty )
      {
        final TimeSeriesComplexType binding = getCrossSectionBinding( node );
        fillEmptyWorkspace( workspace, binding );

        // FIXME save workspace onto disk
      }

      return null;
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  private void fillEmptyWorkspace( final CommandableWorkspace workspace, final TimeSeriesComplexType binding ) throws CoreException
  {
    /* observation of result workspace */
    final IResultTimeSeries handler = new ResultTimeSeriesHandler( workspace.getRootFeature() );
    final IObservation<TupleResult> observation = handler.getObservation();

    /* read out time series binding and fill observation */
    final List<TimeSerieComplexType> series = binding.getSeries();
    if( series.size() != 1 )
      throw new CoreException( StatusUtilities.createErrorStatus( "Parsing / Transformation of TimeSeriesComplexType doesn't support multiple result time series." ) );

    final TupleResult result = observation.getResult();

    for( final TimeSerieComplexType ts : series )
    {
      final List<EventComplexType> events = ts.getEvent();
      for( final EventComplexType event : events )
      {
        /* data */
        final XMLGregorianCalendar date = event.getDate();
        final XMLGregorianCalendar time = event.getTime();

        final GregorianCalendar calendar = new GregorianCalendar();
        calendar.set( GregorianCalendar.DAY_OF_MONTH, date.getDay() );
        calendar.set( GregorianCalendar.MONTH, date.getMonth() );
        calendar.set( GregorianCalendar.YEAR, date.getYear() );
        calendar.set( GregorianCalendar.HOUR_OF_DAY, time.getHour() );
        calendar.set( GregorianCalendar.MINUTE, time.getMinute() );
        calendar.set( GregorianCalendar.SECOND, time.getSecond() );

        final Double value = event.getValue();

        /* new record */
        final IRecord record = result.createRecord();
        record.setValue( 0, calendar.getTime() );
        record.setValue( 1, value );

        result.add( record );
      }
    }
  }

  private void registerWorkspaces( final ICrossSectionNode node, final GMLWorkspace gml, final CommandableWorkspace cmd )
  {
    m_workspaces.put( node, gml );
    m_commandables.put( node, cmd );
  }

  /* dispose existing result workspaces */
  public void dispose( )
  {
    final Collection<CommandableWorkspace> commandables = m_commandables.values();
    for( final CommandableWorkspace workspace : commandables )
    {
      workspace.dispose();
    }

    m_commandables.clear();

    final Collection<GMLWorkspace> workspaces = m_workspaces.values();
    for( final GMLWorkspace workspace : workspaces )
    {
      workspace.dispose();
    }

    m_workspaces.clear();
  }
}
