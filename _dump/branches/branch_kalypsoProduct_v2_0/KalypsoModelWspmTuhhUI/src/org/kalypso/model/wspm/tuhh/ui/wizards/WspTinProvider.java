package org.kalypso.model.wspm.tuhh.ui.wizards;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.gml.provider.GmlSource;
import org.kalypso.core.gml.provider.IGmlSource;
import org.kalypso.core.gml.provider.IGmlSourceProvider;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIImages;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

public class WspTinProvider implements IGmlSourceProvider
{
  public class TinVisitor implements IResourceVisitor
  {
    private final List<IGmlSource> m_tins;

    public TinVisitor( final List<IGmlSource> tins )
    {
      m_tins = tins;
    }

    /**
     * @see org.eclipse.core.resources.IResourceVisitor#visit(org.eclipse.core.resources.IResource)
     */
    public boolean visit( final IResource resource ) throws CoreException
    {
      if( resource instanceof IContainer )
      {
        final IContainer c = (IContainer) resource;
        final IResource member = c.findMember( new Path( "_aktuell/Daten/WspTin.gml" ) );
        if( member instanceof IFile )
        {
          try
          {
            final URL url = ResourceUtilities.createURL( member );
            final String name = c.getProject().getName() + " - " + c.getName();
            final String desc = c.getName();
            final IGmlSource source = new GmlSource( name, desc, url, new GMLXPath( "TriangulatedSurfaceFeature/triangulatedSurfaceMember", null ) );
            m_tins.add( source );

            return false;
          }
          catch( final MalformedURLException e )
          {
            throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
          }
        }
      }

      return true;
    }

  }

  private final ITreeContentProvider m_contentProvider = new ITreeContentProvider()
  {
    private Map<IProject, IGmlSource[]> m_sources = new HashMap<IProject, IGmlSource[]>();

    public Object[] getChildren( Object parentElement )
    {
      // Project: load workspace
      if( parentElement instanceof IProject )
      {
        final IProject project = (IProject) parentElement;
        return tinsForProject( project );
      }

      return new Object[] {};
    }

    private IGmlSource[] tinsForProject( final IProject project )
    {
      try
      {
        if( m_sources.containsKey( project ) )
          return m_sources.get( project );

        final List<IGmlSource> tins = new ArrayList<IGmlSource>();

        final IFolder resultsFolder = project.getFolder( "Ergebnisse" );
        if( resultsFolder.exists() )
        {
          IResourceVisitor tinVisitor = new TinVisitor( tins );
          resultsFolder.accept( tinVisitor, IResource.DEPTH_ONE, false );
        }

        final IGmlSource[] tinArray = tins.toArray( new IGmlSource[tins.size()] );
        m_sources.put( project, tinArray );

        return tinArray;
      }
      catch( CoreException e )
      {
        KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( e.getStatus() );
        return new IGmlSource[] {};
      }
    }

    public Object getParent( Object element )
    {
      if( element instanceof IFile )
        return ((IFile) element).getProject();

      return null;
    }

    public boolean hasChildren( Object element )
    {
      if( element instanceof IProject )
        return true;

      return false;
    }

    public Object[] getElements( Object inputElement )
    {
      final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
      final IProject[] projects = root.getProjects();

      final List<IProject> result = new ArrayList<IProject>( projects.length );

      for( final IProject project : projects )
      {
        if( !project.isOpen() )
          continue;

        // As wspm projects do not have a special nature, we just check for the .gmv file
        final IResource gmvMember = project.findMember( new Path( "WSPM.gmv" ) );
        if( gmvMember != null )
          result.add( project );
      }

      return result.toArray( new IProject[result.size()] );
    }

    public void dispose( )
    {
      m_sources.clear();
    }

    public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
    {
    }
  };

  private final ILabelProvider m_labelProvider = new LabelProvider()
  {
    private final ILabelProvider m_workbenchLP = WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider();

    /**
     * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
     */
    @Override
    public Image getImage( Object element )
    {
      if( element instanceof IResource )
        return m_workbenchLP.getImage( element );

      if( element instanceof IGmlSource )
        return KalypsoModelWspmTuhhUIPlugin.getImageProvider().getImage( KalypsoModelWspmTuhhUIImages.WSP_TIN );

      return super.getImage( element );
    }

    /**
     * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
     */
    @Override
    public String getText( Object element )
    {
      if( element instanceof IResource )
        return m_workbenchLP.getText( element );

      if( element instanceof IGmlSource )
        return ((IGmlSource) element).getDescription();

      return super.getText( element );
    }
  };

  public ITreeContentProvider createContentProvider( )
  {
    return m_contentProvider;
  }

  public ILabelProvider createLabelProvider( )
  {
    return m_labelProvider;
  }

  public void createInfoControl( final Composite parent, final Object element )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );
    final Label label = new Label( composite, SWT.NONE );
    label.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, true, true ) );

    label.setText( "Keine Info verf�gbar" );
  }

  public IGmlSource createSource( final Object element )
  {
    if( element instanceof IGmlSource )
      return (IGmlSource) element;

    return null;
  }

}
