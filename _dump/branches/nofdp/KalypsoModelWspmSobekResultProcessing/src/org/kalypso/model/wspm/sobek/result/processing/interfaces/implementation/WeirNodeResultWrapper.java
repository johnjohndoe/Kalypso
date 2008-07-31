package org.kalypso.model.wspm.sobek.result.processing.interfaces.implementation;

import java.io.InputStream;
import java.net.URL;

import nl.wldelft.fews.pi.TimeSerieComplexType;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode.STRUCTURE_TYPE;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWeirNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWorkspaceCache;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.ResultTimeSeriesHandler;
import org.kalypso.model.wspm.sobek.result.processing.utils.ResultModelHelper;
import org.kalypso.model.wspm.sobek.result.processing.worker.ResultWorker;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.util.pool.PoolHelper;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class WeirNodeResultWrapper implements IWeirNodeResultWrapper
{
  private final IFolder m_resultFolder;

  private final IEmptyNode m_node;

  private final IWorkspaceCache m_cache;

  public WeirNodeResultWrapper( IEmptyNode node, IFolder resultFolder, IWorkspaceCache cache )
  {
    m_cache = cache;
    Assert.isTrue( STRUCTURE_TYPE.eWeir.equals( node.getStructureType() ) );
    m_node = node;
    m_resultFolder = resultFolder;
  }

  public void init( ) throws CoreException
  {
    getDischarge();
    getWaterLevelAbove();
    getWaterLevelBelow();
  }

  private IResultTimeSeries getWaterLevelBelow( )
  {
    // TODO Auto-generated method stub

    return null;
  }

  private IResultTimeSeries getWaterLevelAbove( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  private IResultTimeSeries getDischarge( ) throws CoreException
  {
    try
    {
      final CommandableWorkspace cmd = m_cache.getCommandableWorkspace( m_node.getId() + "_discharge" );
      if( cmd != null )
        return new ResultTimeSeriesHandler( cmd.getRootFeature(), m_node );

      /* get cross section node result file */
      IFile iFile = ResultModelHelper.getWeirNodeResultFile( m_resultFolder, m_node, WEIR_NODE_RESULT.eDischarge ); //$NON-NLS-1$

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

      m_cache.registerWorkspaces( m_node.getId() + "_discharge", gmlWorkspace, workspace );

      /* fill empty workspace with results */
      if( empty )
      {
        final TimeSerieComplexType binding = getNodeBinding( WEIR_NODE_RESULT.eDischarge );
        if( binding == null )
          return null;

        final ResultWorker worker = new ResultWorker( workspace, binding, m_node );
        worker.process();

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

  private TimeSerieComplexType getNodeBinding( WEIR_NODE_RESULT type )
  {
    throw new NotImplementedException();
  }
}
