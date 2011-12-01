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
package org.kalypso.ui.wizards.results.gmlsource;

import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormText;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.afgui.scenarios.IScenario;
import org.kalypso.afgui.views.ScenarioContentProvider;
import org.kalypso.contribs.eclipse.core.resources.ProjectUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeControlListener;
import org.kalypso.core.gml.provider.GmlSource;
import org.kalypso.core.gml.provider.IGmlSource;
import org.kalypso.core.gml.provider.IGmlSourceProvider;
import org.kalypso.kalypso1d2d.pjt.Kalypso1D2DProjectNature;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypso.ui.wizards.results.ResultMetaInfoViewer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * @author Gernot Belger
 */
public class WspTinProvider implements IGmlSourceProvider, ITreeContentProvider
{
  private final ScenarioContentProvider m_scenarioContentProvider = new ScenarioContentProvider( false );

  private final WorkbenchContentProvider m_workbenchContentProvider = new WorkbenchContentProvider();

  private final List<GMLWorkspace> m_workspace = new ArrayList<GMLWorkspace>();

  private final Map<IResultMeta, IScenario> m_resultParents = new HashMap<IResultMeta, IScenario>();

  private static final Object[] NO_CHILDREN = new Object[] {};

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  @Override
  public void dispose( )
  {
    m_scenarioContentProvider.dispose();
    m_workbenchContentProvider.dispose();

    for( final GMLWorkspace workspace : m_workspace )
    {
      workspace.dispose();
    }
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createContentProvider()
   */
  @Override
  public ITreeContentProvider createContentProvider( )
  {
    return this;
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createLabelProvider()
   */
  @Override
  public ILabelProvider createLabelProvider( )
  {
    return WorkbenchLabelProvider.getDecoratingWorkbenchLabelProvider();

  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createInfoControl(org.eclipse.swt.widgets.Composite,
   *      java.lang.Object)
   */
  @Override
  public void createInfoControl( final Composite parent, final Object element )
  {
    if( element instanceof IResultMeta )
    {
      final ScrolledComposite sc = new ScrolledComposite( parent, SWT.V_SCROLL | SWT.H_SCROLL );
      sc.setExpandHorizontal( true );
      sc.setExpandVertical( true );

      final String informationText = ResultMetaInfoViewer.getInformationText( (IResultMeta) element );

      final Composite panel = new Composite( sc, SWT.NONE );
      panel.setLayout( new GridLayout() );

      final FormText label = new FormText( panel, SWT.NONE );
      label.setText( informationText, true, false );
      label.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

      sc.setContent( panel );

      sc.addControlListener( new ScrolledCompositeControlListener( sc ) );
      sc.setMinSize( panel.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_resultParents.clear();
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  @Override
  public Object[] getElements( final Object inputElement )
  {
    return ProjectUtilities.allOfNature( Kalypso1D2DProjectNature.ID );
  }

  /**
   * @see org.kalypso.core.gml.provider.IGmlSourceProvider#createSource(java.lang.Object)
   */
  @Override
  public IGmlSource createSource( final Object element )
  {
    // create source if it is a tin-wsp
    if( element instanceof IDocumentResultMeta )
    {
      final IDocumentResultMeta wspDoc = (IDocumentResultMeta) element;
      if( wspDoc.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.tinWsp )
      {
        try
        {
          final IScenario scenario = findScenario( wspDoc );
          final IFolder scenarioFolder = scenario.getFolder();

          final IPath fullPath = wspDoc.getFullPath();

          final IFile file = scenarioFolder.getFile( fullPath );

          final String name = wspDoc.getName();
          final String description = wspDoc.getDescription();
          final URL location = ResourceUtilities.createURL( file );
          final GMLXPath rootPath = new GMLXPath( Kalypso1D2DSchemaConstants.TIN_RESULT );
          final GMLXPath path = new GMLXPath( rootPath, Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_TIN );

          return new GmlSource( name, description, location, path );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
    }

    return null;
  }

  private IScenario findScenario( final IResultMeta meta )
  {
    if( m_resultParents.containsKey( meta ) )
    {
      return m_resultParents.get( meta );
    }

    return findScenario( meta.getParent() );
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object parentElement )
  {
    try
    {
      if( parentElement instanceof IProject || parentElement instanceof IScenario )
      {
        final List<Object> children = new ArrayList<Object>();

        final Object[] scenarioChildren = m_scenarioContentProvider.getChildren( parentElement );
        children.addAll( Arrays.asList( scenarioChildren ) );

        if( parentElement instanceof IScenario )
        {
          final IScenario scenario = (IScenario) parentElement;

          final IFolder scenarioFolder = scenario.getFolder();
          final IFile resultsFile = scenarioFolder.getFile( Path.fromPortableString( "models/scenarioResultMeta.gml" ) ); //$NON-NLS-1$
          final URL resultUrl = ResourceUtilities.createURL( resultsFile );

          final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( resultUrl, null );
          m_workspace.add( workspace );
          final IResultMeta rootMeta = (IResultMeta) workspace.getRootFeature().getAdapter( IResultMeta.class );

          final IFeatureWrapperCollection<IResultMeta> metaChildren = rootMeta.getChildren();

          final List<IResultMeta> wspChildren = filterMetaChildren( metaChildren );

          /* Remember parent for getParent */
          for( final IResultMeta child : wspChildren )
          {
            m_resultParents.put( child, scenario );
          }

          children.addAll( wspChildren );
        }

        return children.toArray( new Object[children.size()] );
      }

      final Object[] children = m_workbenchContentProvider.getChildren( parentElement );
      return filterMetaChildren( children );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return NO_CHILDREN;
  }

  /**
   * Filters everything which does not contain any wsp.
   */
  private IResultMeta[] filterMetaChildren( final Object[] metaChildren )
  {
    final List<IResultMeta> result = new ArrayList<IResultMeta>();

    for( final Object child : metaChildren )
    {
      if( child instanceof IResultMeta )
      {
        final boolean filtered = filterResultMeta( child );
        if( !filtered )
        {
          result.add( (IResultMeta) child );
        }
      }
      else
      {
        result.add( (IResultMeta) child );
      }
    }

    return result.toArray( new IResultMeta[result.size()] );
  }

  /**
   * Filters everything which does not contain any wsp.
   */
  private List<IResultMeta> filterMetaChildren( final List<IResultMeta> metaChildren )
  {
    final List<IResultMeta> result = new ArrayList<IResultMeta>();

    for( final IResultMeta child : metaChildren )
    {
      final boolean filtered = filterResultMeta( child );
      if( !filtered )
      {
        result.add( child );
      }
    }

    return result;
  }

  private boolean filterResultMeta( final Object child )
  {
    final IResultMeta wspMeta = findFirstWspTinMetaChild( (IResultMeta) child );
    final boolean filtered = wspMeta == null;
    return filtered;
  }

  private IResultMeta findFirstWspTinMetaChild( final IResultMeta resultMeta )
  {
    if( resultMeta instanceof IDocumentResultMeta )
    {
      final IDocumentResultMeta docMeta = (IDocumentResultMeta) resultMeta;
      if( docMeta.getDocumentType() == IDocumentResultMeta.DOCUMENTTYPE.tinWsp )
      {
        return docMeta;
      }
    }

    final IFeatureWrapperCollection<IResultMeta> children = resultMeta.getChildren();
    for( final IResultMeta child : children )
    {
      final IResultMeta childsWspDoc = findFirstWspTinMetaChild( child );
      if( childsWspDoc != null )
      {
        return childsWspDoc;
      }
    }

    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object element )
  {
    if( element instanceof IProject )
    {
      return null;
    }

    if( element instanceof IScenario )
    {
      final IScenario s = (IScenario) element;
      final IScenario parentScenario = s.getParentScenario();
      if( parentScenario != null )
      {
        return parentScenario;
      }

      return s.getProject();
    }

    if( element instanceof IResultMeta )
    {
      final IResultMeta result = (IResultMeta) element;
      final IResultMeta parentResult = result.getParent();
      if( parentResult != null )
      {
        return parentResult;
      }

      if( m_resultParents.containsKey( result ) )
      {
        return m_resultParents.get( result );
      }
    }

    throw new IllegalStateException( Messages.getString( "org.kalypso.ui.wizards.results.gmlsource.WspTinProvider.1" ) + element ); //$NON-NLS-1$
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  @Override
  public boolean hasChildren( final Object element )
  {
    if( element instanceof IProject || element instanceof IScenario )
    {
      if( m_scenarioContentProvider.hasChildren( element ) )
      {
        return true;
      }

      if( element instanceof IScenario )
      {
        return true;
      }
    }

    return m_workbenchContentProvider.hasChildren( element );
  }
}
