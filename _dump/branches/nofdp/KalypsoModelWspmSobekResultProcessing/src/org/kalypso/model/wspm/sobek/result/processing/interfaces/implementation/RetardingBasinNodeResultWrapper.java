package org.kalypso.model.wspm.sobek.result.processing.interfaces.implementation;

import java.io.InputStream;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

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
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode.STRUCTURE_TYPE;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IRetardingBasinNodeResultWrapper;
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

public class RetardingBasinNodeResultWrapper implements IRetardingBasinNodeResultWrapper
{
  private static final QName QN_IS_CONTROLLED = new QName( ISobekConstants.NS_NOFPD_1D_MODEL, "isControlledRetardingBasin" );//$NON-NLS-1$

  private final IFolder m_resultFolder;

  private final IEmptyNode m_node;

  private final IWorkspaceCache m_cache;

  public RetardingBasinNodeResultWrapper( IEmptyNode node, IFolder resultFolder, IWorkspaceCache cache )
  {
    Assert.isTrue( STRUCTURE_TYPE.eRetardingBasin.equals( node.getStructureType() ) );

    m_node = node;
    m_resultFolder = resultFolder;
    m_cache = cache;
  }

  public void init( ) throws CoreException
  {
    getWaterLevel();
    getHochwasserEntlastung();
    getGrundAblass();
  }

  private IResultTimeSeries getTimeSeries( RETARDING_BASIN_NODE_RESULT type, IResultWorkerSettings settings ) throws CoreException
  {
    try
    {
      final CommandableWorkspace cmd = m_cache.getCommandableWorkspace( String.format( "%s%s", m_node.getId(), type.getPostfix() ) );
      if( cmd != null )
        return new ResultTimeSeriesHandler( cmd.getRootFeature(), m_node );

      /* get cross section node result file */
      IFile iFile = ResultModelHelper.getRetardingBasinResultFile( m_resultFolder, m_node, type ); //$NON-NLS-1$

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

  public TimeSerieComplexType getNodeBinding( RETARDING_BASIN_NODE_RESULT type ) throws CoreException
  {
    JAXBElement<TimeSeriesComplexType> jaxRoot;

    final String id;
    if( RETARDING_BASIN_NODE_RESULT.eWaterLevel.equals( type ) )
    {
      jaxRoot = m_cache.getPiCalculationPointElement();
      id = String.format( "%s_ln", m_node.getId() );
    }
    else if( RETARDING_BASIN_NODE_RESULT.eHochwasserEntlastung.equals( type ) )
    {
      jaxRoot = m_cache.getPiStructuresElement();
      id = String.format( "%s##rW", m_node.getId() );
    }
    else if( RETARDING_BASIN_NODE_RESULT.eGrundAblass.equals( type ) )
    {
      jaxRoot = m_cache.getPiStructuresElement();

      if( isControlled() == true )
      {
        id = String.format( "%s##dbS", m_node.getId() );
      }
      else
      {
        id = String.format( "%s##gS", m_node.getId() );
      }
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
        String myParamId;
        if( RETARDING_BASIN_NODE_RESULT.eWaterLevel.equals( type ) )
        {
          myParamId = "Waterlevel  (m AD)"; // achtung: 2 leerzeichen!
        }
        else if( RETARDING_BASIN_NODE_RESULT.eHochwasserEntlastung.equals( type ) )
        {
          myParamId = "Discharge (m³/s)";
        }
        else if( RETARDING_BASIN_NODE_RESULT.eGrundAblass.equals( type ) )
        {
          myParamId = "Discharge (m³/s)";
        }
        else
          throw new IllegalStateException();

        if( myParamId.equals( parameterId ) )
          return complex;
      }

    }

    return null;
  }

  public IResultTimeSeries getGrundAblass( ) throws CoreException
  {
    return getTimeSeries( RETARDING_BASIN_NODE_RESULT.eGrundAblass, new IResultWorkerSettings()
    {
      public String getParameterId( )
      {
        return "Q";
      }

      public String getUnit( )
      {
        return "m³/s";
      }
    } );
  }

  public IResultTimeSeries getHochwasserEntlastung( ) throws CoreException
  {
    return getTimeSeries( RETARDING_BASIN_NODE_RESULT.eHochwasserEntlastung, new IResultWorkerSettings()
    {
      public String getParameterId( )
      {
        return "Q";
      }

      public String getUnit( )
      {
        return "m³/s";
      }
    } );
  }

  public IResultTimeSeries getWaterLevel( ) throws CoreException
  {
    return getTimeSeries( RETARDING_BASIN_NODE_RESULT.eWaterLevel, new IResultWorkerSettings()
    {
      public String getParameterId( )
      {
        return "W";
      }

      public String getUnit( )
      {
        return "m NHN";
      }
    } );
  }

  public boolean isControlled( )
  {
    return (Boolean) m_node.getFeature().getProperty( QN_IS_CONTROLLED );
  }
}
