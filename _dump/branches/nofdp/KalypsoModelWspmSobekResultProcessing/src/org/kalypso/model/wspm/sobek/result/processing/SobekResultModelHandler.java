package org.kalypso.model.wspm.sobek.result.processing;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import nl.wldelft.fews.pi.HeaderComplexType;
import nl.wldelft.fews.pi.TimeSerieComplexType;
import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.IConnectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IEmptyNode;
import org.kalypso.model.wspm.sobek.core.interfaces.INode;
import org.kalypso.model.wspm.sobek.result.processing.i18n.Messages;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IPolderNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IRetardingBasinNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.ISobekResultModel;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWeirNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.IWorkspaceCache;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.implementation.PolderNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.implementation.RetardingBasinNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.interfaces.implementation.WeirNodeResultWrapper;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchLengthSectionModel;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.BranchLengthSectionsHandler;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.NodeTimeSeriesHandler;
import org.kalypso.model.wspm.sobek.result.processing.utils.ResultModelHelper;
import org.kalypso.model.wspm.sobek.result.processing.worker.IResultWorkerSettings;
import org.kalypso.model.wspm.sobek.result.processing.worker.ResultWorker;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.util.pool.PoolHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class SobekResultModelHandler implements ISobekResultModel, IWorkspaceCache
{
  private static JAXBContext JC = null;

  private final IFolder m_resultFolder;

  private static final Map<String, GMLWorkspace> m_workspaces = new HashMap<String, GMLWorkspace>();

  private static final Map<String, CommandableWorkspace> m_commandables = new HashMap<String, CommandableWorkspace>();

  private GMLWorkspace m_branchesWorkspace;

  private CommandableWorkspace m_branchesCommandableWorkspace;

  private JAXBElement<TimeSeriesComplexType> m_jaxPiCalculationPointRoot;

  private JAXBElement<TimeSeriesComplexType> m_jaxPiStructuresRoot;

  private SobekResultModelHandler( final IFolder resultFolder ) throws JAXBException
  {
    m_resultFolder = resultFolder;

    if( JC == null )
    {
      JC = JAXBContext.newInstance( nl.wldelft.fews.pi.ObjectFactory.class );
    }
  }

  public JAXBContext getJaxBContext( )
  {
    return JC;
  }

  /**
   * @param resultFolder
   *            Sobek result folder
   * @return Sobek Result Model
   * @throws JAXBException
   */
  public static ISobekResultModel getHandler( final IFolder resultFolder ) throws CoreException, JAXBException
  {
    if( !resultFolder.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.SobekResultModelHandler_0 ) );

    return new SobekResultModelHandler( resultFolder );
  }

  public void registerWorkspaces( final String id, final GMLWorkspace gml, final CommandableWorkspace cmd )
  {
    m_workspaces.put( id, gml );
    m_commandables.put( id, cmd );
  }

  /* dispose existing result workspaces */
  public void dispose( )
  {
    m_jaxPiCalculationPointRoot = null;
    m_jaxPiStructuresRoot = null;

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

    if( m_branchesWorkspace != null )
      m_branchesWorkspace.dispose();

    if( m_branchesCommandableWorkspace != null )
      m_branchesCommandableWorkspace.dispose();

    m_branchesWorkspace = null;
    m_branchesCommandableWorkspace = null;
  }

  public IBranchLengthSectionModel getBranchHydrographModel( ) throws CoreException
  {
    try
    {
      if( m_branchesCommandableWorkspace == null )
      {
        /* parse gml workspace */
        final IFile file = ResultModelHelper.getBranchHydrogrographWorkspaceFile( m_resultFolder );
        final URL url = file.getRawLocationURI().toURL();

        m_branchesWorkspace = GmlSerializer.createGMLWorkspace( url, null );
        m_branchesCommandableWorkspace = PoolHelper.getCommandableWorkspace( m_branchesWorkspace );
      }

      final Feature root = m_branchesCommandableWorkspace.getRootFeature();
      final Object property = root.getProperty( IBranchLengthSectionModel.QN_HYDROGRAPHS );

      if( property instanceof FeatureList )
        return new BranchLengthSectionsHandler( this, m_branchesCommandableWorkspace, (FeatureList) property );

      return null;
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  public TimeSerieComplexType getNodeBinding( INode node ) throws CoreException
  {
    Assert.isTrue( !(node instanceof IEmptyNode) ); // structure nodes need special handling!

    /* look for id? */
    final String id;
    if( node instanceof ICrossSectionNode )
    {
      id = String.format( "C%s", node.getId() ); //$NON-NLS-1$
    }
    else if( node instanceof IConnectionNode )
    {
      // TODO handling of IBoundaryNodes.W
      id = String.format( "%s", node.getId() ); //$NON-NLS-1$
    }
    else
      throw new NotImplementedException();

    TimeSeriesComplexType values = getPiCalculationPointElement().getValue();
    List<TimeSerieComplexType> series = values.getSeries();
    for( TimeSerieComplexType complex : series )
    {
      HeaderComplexType header = complex.getHeader();
      String locationId = header.getLocationId();

      if( locationId.equals( id ) ) //$NON-NLS-1$
        return complex;
    }

    return null;
  }

  public IResultTimeSeries getNodeTimeSeries( INode node ) throws CoreException
  {
    Assert.isTrue( !(node instanceof IEmptyNode) ); // structure nodes need special handling!

    try
    {
      final CommandableWorkspace cmd = m_commandables.get( node.getId() );
      if( cmd != null )
        return new NodeTimeSeriesHandler( cmd.getRootFeature(), node );

      /* get cross section node result file */
      IFile iFile = ResultModelHelper.getNodeResultFile( m_resultFolder, node ); //$NON-NLS-1$

      boolean empty = false;

      /* gml file doesn't exists - create a new empty gml (workspace) file */
      if( !iFile.exists() )
      {
        final InputStream stream = this.getClass().getResourceAsStream( "utils/templates/templateSobekResultTimeSeries.gml" ); //$NON-NLS-1$
        iFile.create( stream, true, new NullProgressMonitor() );
        stream.close();

        empty = true;
      }

      /* parse gml workspace */
      final URL url = iFile.getRawLocationURI().toURL();

      final GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( url, null );
      final CommandableWorkspace workspace = PoolHelper.getCommandableWorkspace( gmlWorkspace );

      registerWorkspaces( node.getId(), gmlWorkspace, workspace );

      if( empty )
      {
        /* fill empty workspace with results */
        final TimeSerieComplexType binding = getNodeBinding( node );
        if( binding == null )
          return null;

        NodeTimeSeriesHandler handler = new NodeTimeSeriesHandler( workspace.getRootFeature(), node );

        final ResultWorker worker = new ResultWorker( workspace, binding, node, handler );
        worker.process( new IResultWorkerSettings()
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

        // save changes
        GmlSerializer.serializeWorkspace( iFile.getLocation().toFile(), workspace, "UTF-8" ); //$NON-NLS-1$
      }

      return new NodeTimeSeriesHandler( workspace.getRootFeature(), node );
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  public IFolder getResultFolder( )
  {
    return m_resultFolder;
  }

  public IPolderNodeResultWrapper getPolderNodeResult( IEmptyNode node )
  {
    return new PolderNodeResultWrapper( node, m_resultFolder, this );
  }

  public IRetardingBasinNodeResultWrapper getRetardingBasinNodeResult( IEmptyNode node )
  {
    return new RetardingBasinNodeResultWrapper( node, m_resultFolder, this );
  }

  public IWeirNodeResultWrapper getWeirNodeResult( IEmptyNode node )
  {
    return new WeirNodeResultWrapper( node, m_resultFolder, this );
  }

  public CommandableWorkspace getCommandableWorkspace( String id )
  {
    return m_commandables.get( id );
  }

  public JAXBElement<TimeSeriesComplexType> getPiCalculationPointElement( ) throws CoreException
  {
    if( m_jaxPiCalculationPointRoot == null )
    {
      final IFile iFile = ResultModelHelper.getPiCalculationPointFile( m_resultFolder );
      final InputStream is = new BufferedInputStream( iFile.getContents() );

      try
      {
        final Unmarshaller u = JC.createUnmarshaller();
        m_jaxPiCalculationPointRoot = (JAXBElement<TimeSeriesComplexType>) u.unmarshal( is );

        return m_jaxPiCalculationPointRoot;
      }
      catch( final Exception e )
      {
        throw new CoreException( StatusUtilities.createErrorStatus( Messages.SobekResultModelHandler_4 ) );
      }
      finally
      {
        try
        {
          is.close();
        }
        catch( final IOException e )
        {
          throw new CoreException( StatusUtilities.createWarningStatus( Messages.SobekResultModelHandler_5 ) );
        }
      }
    }

    return m_jaxPiCalculationPointRoot;
  }

  public JAXBElement<TimeSeriesComplexType> getPiStructuresElement( ) throws CoreException
  {
    if( m_jaxPiStructuresRoot == null )
    {
      final IFile iFile = ResultModelHelper.getPiStructureFile( m_resultFolder );
      final InputStream is = new BufferedInputStream( iFile.getContents() );

      try
      {
        final Unmarshaller u = JC.createUnmarshaller();
        m_jaxPiStructuresRoot = (JAXBElement<TimeSeriesComplexType>) u.unmarshal( is );

        return m_jaxPiStructuresRoot;
      }
      catch( final Exception e )
      {
        throw new CoreException( StatusUtilities.createErrorStatus( Messages.SobekResultModelHandler_4 ) );
      }
      finally
      {
        try
        {
          is.close();
        }
        catch( final IOException e )
        {
          throw new CoreException( StatusUtilities.createWarningStatus( Messages.SobekResultModelHandler_5 ) );
        }
      }
    }

    return m_jaxPiStructuresRoot;
  }
}
