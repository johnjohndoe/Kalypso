package org.kalypso.model.wspm.sobek.result.processing;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import nl.wldelft.fews.pi.TimeSeriesComplexType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.sobek.core.interfaces.ICrossSectionNode;
import org.kalypso.model.wspm.sobek.result.processing.i18n.Messages;
import org.kalypso.model.wspm.sobek.result.processing.model.IBranchHydrographModel;
import org.kalypso.model.wspm.sobek.result.processing.model.IResultTimeSeries;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.BranchHydraphModelHandler;
import org.kalypso.model.wspm.sobek.result.processing.model.implementation.ResultTimeSeriesHandler;
import org.kalypso.model.wspm.sobek.result.processing.worker.ResultWorker;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.util.pool.PoolHelper;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class SobekResultModelHandler implements ISobekResultModel
{
  private static JAXBContext JC = null;

  private final IFolder m_resultFolder;

  private static final Map<ICrossSectionNode, GMLWorkspace> m_workspaces = new HashMap<ICrossSectionNode, GMLWorkspace>();

  private static final Map<ICrossSectionNode, CommandableWorkspace> m_commandables = new HashMap<ICrossSectionNode, CommandableWorkspace>();

  private GMLWorkspace m_branchesWorkspace;

  private CommandableWorkspace m_branchesCommandableWorkspace;

  private SobekResultModelHandler( final IFolder resultFolder ) throws JAXBException
  {
    m_resultFolder = resultFolder;

    if( JC == null )
    {
      JC = JAXBContext.newInstance( nl.wldelft.fews.pi.ObjectFactory.class );
    }

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

  private IFile getCrossSectionNodeResultFile( final ICrossSectionNode node, final String extension ) throws CoreException
  {

    final IFolder folder = m_resultFolder.getFolder( "output/nodes/csn/" ); //$NON-NLS-1$
    if( !folder.exists() )
      throw new CoreException( StatusUtilities.createErrorStatus( Messages.SobekResultModelHandler_2 ) );

    final IFile iFile = folder.getFile( node.getId() + extension );
    if( !iFile.exists() )
      return null;

    return iFile;
  }

  public TimeSeriesComplexType getCrossSectionBinding( final ICrossSectionNode node ) throws CoreException
  {
    final IFile iFile = getCrossSectionNodeResultFile( node, ".xml" ); //$NON-NLS-1$

    final InputStream is = new BufferedInputStream( iFile.getContents() );
    try
    {
      final Unmarshaller u = JC.createUnmarshaller();
      final JAXBElement<TimeSeriesComplexType> jaxRoot = (JAXBElement<TimeSeriesComplexType>) u.unmarshal( is );

      return jaxRoot.getValue();
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

  public IResultTimeSeries getCrossSectionTimeSeries( final ICrossSectionNode node ) throws CoreException
  {
    try
    {
      final CommandableWorkspace cmd = m_commandables.get( node );
      if( cmd != null )
        return new ResultTimeSeriesHandler( cmd.getRootFeature() );

      /* get cross section node result file */
      IFile iFile = getCrossSectionNodeResultFile( node, ".gml" ); //$NON-NLS-1$

      boolean empty = false;

      /* gml file doesn't exists - create a new empty gml (workspace) file */
      if( iFile == null )
      {
        final IFile iCSN = getCrossSectionNodeResultFile( node, ".xml" ); //$NON-NLS-1$

        final IFolder parent = (IFolder) iCSN.getParent();
        iFile = parent.getFile( node.getId() + ".gml" ); //$NON-NLS-1$

        final InputStream stream = this.getClass().getResourceAsStream( "templates/templateSobekResultTimeSeries.gml" ); //$NON-NLS-1$
        iFile.create( stream, true, new NullProgressMonitor() );
        stream.close();

        empty = true;
      }

      /* parse gml workspace */
      final URL url = iFile.getRawLocationURI().toURL();

      final GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( url, null );
      final CommandableWorkspace workspace = PoolHelper.getCommandableWorkspace( gmlWorkspace );

      registerWorkspaces( node, gmlWorkspace, workspace );

      /* fill empty workspace with results */
      if( empty )
      {
        final TimeSeriesComplexType binding = getCrossSectionBinding( node );

        final ResultWorker worker = new ResultWorker( workspace, binding, node );
        worker.process();

        // save changes
        GmlSerializer.serializeWorkspace( iFile.getLocation().toFile(), workspace, "UTF-8" ); //$NON-NLS-1$
      }

      return new ResultTimeSeriesHandler( workspace.getRootFeature() );
    }
    catch( final Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
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

    m_branchesWorkspace.dispose();
    m_branchesCommandableWorkspace.dispose();

    m_branchesWorkspace = null;
    m_branchesCommandableWorkspace = null;
  }

  public IBranchHydrographModel getBranchHydrographModel( ) throws CoreException
  {
    try
    {
      if( m_branchesCommandableWorkspace == null )
      {
        /* parse gml workspace */
        IFile file = getBranchHydrogrographWorkspaceFile();
        final URL url = file.getRawLocationURI().toURL();

        m_branchesWorkspace = GmlSerializer.createGMLWorkspace( url, null );
        m_branchesCommandableWorkspace = PoolHelper.getCommandableWorkspace( m_branchesWorkspace );
      }

      Feature root = m_branchesCommandableWorkspace.getRootFeature();
      Object property = root.getProperty( IBranchHydrographModel.QN_HYDROGRAPHS );

      if( property instanceof FeatureList )
        return new BranchHydraphModelHandler( this, m_branchesCommandableWorkspace, (FeatureList) property );

      return null;
    }
    catch( Exception e )
    {
      throw new CoreException( StatusUtilities.createErrorStatus( e.getMessage() ) );
    }
  }

  public IFile getBranchHydrogrographWorkspaceFile( ) throws CoreException
  {
    final IFolder folder = m_resultFolder.getFolder( "output/branches/" ); //$NON-NLS-1$
    IFile iBase;
    if( !folder.exists() )
    {
      folder.create( true, false, new NullProgressMonitor() );

      iBase = folder.getFile( "hydrograhps.gml" ); //$NON-NLS-1$

      final InputStream stream = this.getClass().getResourceAsStream( "templates/templateSobekBranchHydrograph.gml" ); //$NON-NLS-1$
      iBase.create( stream, true, new NullProgressMonitor() );
    }
    else
    {
      iBase = folder.getFile( "hydrograhps.gml" ); //$NON-NLS-1$
    }

    return iBase;
  }
}
