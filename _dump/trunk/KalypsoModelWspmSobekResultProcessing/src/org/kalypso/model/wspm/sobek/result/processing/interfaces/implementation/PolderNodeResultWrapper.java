package org.kalypso.model.wspm.sobek.result.processing.interfaces.implementation;

import java.io.InputStream;
import java.net.URL;
import java.util.GregorianCalendar;
import java.util.List;

import javax.xml.bind.JAXBElement;

import nl.wldelft.fews.pi.HeaderComplexType;
import nl.wldelft.fews.pi.TimeSerieComplexType;
import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode.STRUCTURE_TYPE;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IPolderNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWorkspaceCache;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.PolderNodeTimeSeriesHandler;
import org.kalypso.model.wspm.sobek.result.processing.utils.ResultModelHelper;
import org.kalypso.model.wspm.sobek.result.processing.worker.IResultWorkerSettings;
import org.kalypso.model.wspm.sobek.result.processing.worker.ResultWorker;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class PolderNodeResultWrapper implements IPolderNodeResultWrapper
{
  private final IFolder m_resultFolder;

  private final IEmptyNode m_node;

  private final IWorkspaceCache m_cache;

  private final GregorianCalendar m_start;

  private final GregorianCalendar m_end;

  public PolderNodeResultWrapper( IEmptyNode node, IFolder resultFolder, IWorkspaceCache cache, GregorianCalendar start, GregorianCalendar end )
  {
    m_start = start;
    m_end = end;
    Assert.isTrue( STRUCTURE_TYPE.ePolder.equals( node.getStructureType() ) );

    m_node = node;
    m_resultFolder = resultFolder;
    m_cache = cache;
  }

  public void init( ) throws CoreException
  {
    getInflow();
    getOutflow();

  }

  public IResultTimeSeries getInflow( ) throws CoreException
  {
    return getTimeSeries( POLDER_NODE_RESULT.eInflow, new IResultWorkerSettings()
    {
      public String getParameterId( )
      {
        return "Q"; //$NON-NLS-1$
      }

      public String getUnit( )
      {
        return "m³/s"; //$NON-NLS-1$
      }
    } );
  }

  public IResultTimeSeries getOutflow( ) throws CoreException
  {
    return getTimeSeries( POLDER_NODE_RESULT.eOutflow, new IResultWorkerSettings()
    {
      public String getParameterId( )
      {
        return "Q"; //$NON-NLS-1$
      }

      public String getUnit( )
      {
        return "m³/s"; //$NON-NLS-1$
      }
    } );
  }

  public TimeSerieComplexType getNodeBinding( POLDER_NODE_RESULT type ) throws CoreException
  {
    JAXBElement<TimeSeriesComplexType> jaxRoot = m_cache.getPiStructuresElement();

    final String id;
    if( POLDER_NODE_RESULT.eInflow.equals( type ) )
    {
      id = String.format( "%s_w", m_node.getId() );
    }
    else if( POLDER_NODE_RESULT.eOutflow.equals( type ) )
    {
      id = String.format( "%s_p", m_node.getId() );
    }
    else
      throw new IllegalStateException();

    TimeSeriesComplexType values = jaxRoot.getValue();
    List<TimeSerieComplexType> series = values.getSeries();
    for( TimeSerieComplexType complex : series )
    {
      HeaderComplexType header = complex.getHeader();
      String locationId = header.getLocationId();

      if( locationId.equals( id ) ) //$NON-NLS-1$
      {
        String parameterId = header.getParameterId();
        String myParamId = "Discharge (m³/s)";

        if( myParamId.equals( parameterId ) )
          return complex;
      }
    }

    return null;
  }

  private IResultTimeSeries getTimeSeries( POLDER_NODE_RESULT type, IResultWorkerSettings settings ) throws CoreException
  {
    try
    {
      final CommandableWorkspace cmd = m_cache.getCommandableWorkspace( String.format( "%s%s", m_node.getId(), type.getPostfix() ) ); //$NON-NLS-1$
      if( cmd != null )
        return new PolderNodeTimeSeriesHandler( type, cmd.getRootFeature(), m_node );

      /* get cross section node result file */
      IFile iFile = ResultModelHelper.getPolderNodeResultFile( m_resultFolder, m_node, type ); //$NON-NLS-1$

      boolean empty = false;

      /* gml file doesn't exists - create a new empty gml (workspace) file */
      if( !iFile.exists() )
      {
        final InputStream stream = ResultModelHelper.class.getResourceAsStream( "templates/templateSobekResultTimeSeries.gml" ); //$NON-NLS-1$
        iFile.create( stream, true, new NullProgressMonitor() );
        stream.close();

        empty = true;
      }

      /* parse gml workspace */
      final URL url = iFile.getRawLocationURI().toURL();

      final GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( url, null );

      CommandableWorkspace commandable;
      if( gmlWorkspace instanceof CommandableWorkspace )
        commandable = (CommandableWorkspace) gmlWorkspace;
      else
        commandable = new CommandableWorkspace( gmlWorkspace );

      m_cache.registerWorkspaces( String.format( "%s%s", m_node.getId(), type.getPostfix() ), gmlWorkspace, commandable ); //$NON-NLS-1$

      /* fill empty workspace with results */
      if( empty )
      {
        final TimeSerieComplexType binding = getNodeBinding( type );
        if( binding == null )
          return null;

        PolderNodeTimeSeriesHandler handler = new PolderNodeTimeSeriesHandler( type, commandable.getRootFeature(), m_node );

        final ResultWorker worker = new ResultWorker( commandable, binding, m_node, handler, m_start, m_end );
        worker.process( settings );

        // save changes
        GmlSerializer.serializeWorkspace( iFile.getLocation().toFile(), commandable, "UTF-8" ); //$NON-NLS-1$
      }

      return new PolderNodeTimeSeriesHandler( type, commandable.getRootFeature(), m_node );
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }
}
