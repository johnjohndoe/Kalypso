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
import org.kalypso.model.wspm.sobek.result.processing.i18n.Messages;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWeirNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWorkspaceCache;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.WeirTimeSeriesHandler;
import org.kalypso.model.wspm.sobek.result.processing.utils.ResultModelHelper;
import org.kalypso.model.wspm.sobek.result.processing.worker.IResultWorkerSettings;
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

  public IResultTimeSeries getWaterLevelBelow( ) throws CoreException
  {
    return getTimeSeries( WEIR_NODE_RESULT.eWaterLevelBelow, new IResultWorkerSettings()
    {
      public String getParameterId( )
      {
        return "W"; //$NON-NLS-1$
      }

      public String getUnit( )
      {
        return "m NHN"; //$NON-NLS-1$
      }
    } );
  }

  public IResultTimeSeries getWaterLevelAbove( ) throws CoreException
  {
    return getTimeSeries( WEIR_NODE_RESULT.eWaterLevelAbove, new IResultWorkerSettings()
    {
      public String getParameterId( )
      {
        return "W"; //$NON-NLS-1$
      }

      public String getUnit( )
      {
        return "m NHN"; //$NON-NLS-1$
      }
    } );
  }

  private IResultTimeSeries getTimeSeries( WEIR_NODE_RESULT type, IResultWorkerSettings settings ) throws CoreException
  {
    try
    {
      final CommandableWorkspace cmd = m_cache.getCommandableWorkspace( String.format( "%s%s", m_node.getId(), type.getPostfix() ) ); //$NON-NLS-1$
      if( cmd != null )
        return new WeirTimeSeriesHandler( type, cmd.getRootFeature(), m_node );

      /* get cross section node result file */
      IFile iFile = ResultModelHelper.getWeirNodeResultFile( m_resultFolder, m_node, type ); //$NON-NLS-1$

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

      m_cache.registerWorkspaces( String.format( "%s%s", m_node.getId(), type.getPostfix() ), gmlWorkspace, workspace ); //$NON-NLS-1$

      /* fill empty workspace with results */
      if( empty )
      {
        final TimeSerieComplexType binding = getNodeBinding( type );
        if( binding == null )
          return null;

        WeirTimeSeriesHandler handler = new WeirTimeSeriesHandler( type, workspace.getRootFeature(), m_node );

        final ResultWorker worker = new ResultWorker( workspace, binding, m_node, handler );
        worker.process( settings );

        // save changes
        GmlSerializer.serializeWorkspace( iFile.getLocation().toFile(), workspace, "UTF-8" ); //$NON-NLS-1$
      }

      return new WeirTimeSeriesHandler( type, workspace.getRootFeature(), m_node );
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  public IResultTimeSeries getDischarge( ) throws CoreException
  {
    return getTimeSeries( WEIR_NODE_RESULT.eDischarge, new IResultWorkerSettings()
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

  public TimeSerieComplexType getNodeBinding( WEIR_NODE_RESULT type ) throws CoreException
  {
    JAXBElement<TimeSeriesComplexType> jaxRoot = m_cache.getPiStructuresElement();

    final String id = String.format( "%s", m_node.getId() ); //$NON-NLS-1$

    TimeSeriesComplexType values = jaxRoot.getValue();
    List<TimeSerieComplexType> series = values.getSeries();
    for( TimeSerieComplexType complex : series )
    {
      HeaderComplexType header = complex.getHeader();
      String locationId = header.getLocationId();

      if( locationId.equals( id ) ) //$NON-NLS-1$
      {

        String parameterId = header.getParameterId();
        String myParamId;
        if( WEIR_NODE_RESULT.eDischarge.equals( type ) )
        {
          myParamId = Messages.WeirNodeResultWrapper_9;
        }
        else if( WEIR_NODE_RESULT.eWaterLevelAbove.equals( type ) )
        {
          myParamId = Messages.WeirNodeResultWrapper_10;
        }
        else if( WEIR_NODE_RESULT.eWaterLevelBelow.equals( type ) )
        {
          myParamId = Messages.WeirNodeResultWrapper_11;
        }
        else
          throw new IllegalStateException();

        if( myParamId.equals( parameterId ) )
          return complex;
      }

    }

    return null;
  }
}
