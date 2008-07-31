package org.kalypso.model.wspm.sobek.result.processing.interfaces.implementation;

import java.io.InputStream;
import java.net.URL;
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
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.ResultTimeSeriesHandler;
import org.kalypso.model.wspm.sobek.result.processing.utils.ResultModelHelper;
import org.kalypso.model.wspm.sobek.result.processing.worker.IResultWorkerSettings;
import org.kalypso.model.wspm.sobek.result.processing.worker.ResultWorker;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.util.pool.PoolHelper;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class PolderNodeResultWrapper implements IPolderNodeResultWrapper
{
  private final IFolder m_resultFolder;

  private final IEmptyNode m_node;

  private final IWorkspaceCache m_cache;

  public PolderNodeResultWrapper( IEmptyNode node, IFolder resultFolder, IWorkspaceCache cache )
  {

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
        return "Q";
      }

      public String getUnit( )
      {
        return "m�/s";
      }
    } );
  }

  public IResultTimeSeries getOutflow( ) throws CoreException
  {
    return getTimeSeries( POLDER_NODE_RESULT.eOutflow, new IResultWorkerSettings()
    {
      public String getParameterId( )
      {
        return "Q";
      }

      public String getUnit( )
      {
        return "m�/s";
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
        String myParamId = "Discharge (m�/s)";

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
      final CommandableWorkspace cmd = m_cache.getCommandableWorkspace( String.format( "%s%s", m_node.getId(), type.getPostfix() ) );
      if( cmd != null )
        return new ResultTimeSeriesHandler( cmd.getRootFeature(), m_node );

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
      final CommandableWorkspace workspace = PoolHelper.getCommandableWorkspace( gmlWorkspace );

      m_cache.registerWorkspaces( String.format( "%s%s", m_node.getId(), type.getPostfix() ), gmlWorkspace, workspace );

      /* fill empty workspace with results */
      if( empty )
      {
        final TimeSerieComplexType binding = getNodeBinding( type );
        if( binding == null )
          return null;

        final ResultWorker worker = new ResultWorker( workspace, binding, m_node );
        worker.process( settings );

        // save changes
        GmlSerializer.serializeWorkspace( iFile.getLocation().toFile(), workspace, "UTF-8" ); //$NON-NLS-1$
      }

      return new ResultTimeSeriesHandler( workspace.getRootFeature(), m_node );
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }
}
