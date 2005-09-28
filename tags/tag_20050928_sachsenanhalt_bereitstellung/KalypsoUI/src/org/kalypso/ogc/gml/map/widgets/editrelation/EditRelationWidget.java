/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.widgets.editrelation;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.kalypso.commons.command.ICommand;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.command.AddHeavyRelationshipCommand;
import org.kalypso.ui.editor.gmleditor.util.command.AddRelationCommand;
import org.kalypso.ui.editor.gmleditor.util.command.RemoveHeavyRelationCommand;
import org.kalypso.ui.editor.gmleditor.util.command.RemoveRelationCommand;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Annotation;
import org.kalypsodeegree.model.feature.CascadingFeatureList;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FindExistingHeavyRelationsFeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Widget where the user can create relations between selected features. only features from the workspace of the active
 * featuretheme can be selected <br>
 * Constraints from gml-application schemas are supported.
 * 
 * TODO use check icons that indicate mixed child status <br>
 * TODO support removing relations <br>
 * 
 * @author doemming
 */
public class EditRelationWidget extends AbstractWidget implements IWidgetWithOptions
{
  final String[] m_modeItems = new String[]
  {
      "Relation erstellen",
      "Relationen entfernen" };

  Feature m_srcFE = null;

  Feature m_targetFE = null;

  private FeatureList m_allowedFeatureList = null;

  private static final double RADIUS = 30;

  private TreeViewer m_viewer;

  final EditRelationOptionsContentProvider m_contentProvider = new EditRelationOptionsContentProvider();

  Composite m_topLevel;

  Text m_textInfo;

  Text m_textProblem;

  final StringBuffer m_fitProblems = new StringBuffer();

  private Combo m_modeCombo;

  public static final int MODE_ADD = 0;

  public static final int MODE_REMOVE = 1;

  int m_modificationMode = MODE_ADD;

  EditRelationOptionsLabelProvider m_labelProvider = null;

  public EditRelationWidget( String name, String toolTip )
  {
    super( name, toolTip );
  }

  public void leftPressed( Point p )
  {
    if( m_srcFE != null && m_targetFE != null )
    {
      perform();
      finish();
      return;
    }
    m_targetFE = null;
    final JMSelector selector = new JMSelector();
    final MapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final GM_Point point = GeometryFactory
        .createGM_Point( p, transform, mapPanel.getMapModell().getCoordinatesSystem() );

    final double r = transform.getSourceX( RADIUS ) - transform.getSourceX( 0 );

    final Feature feature = selector.selectNearest( point, r, m_allowedFeatureList, false );
    m_srcFE = feature;
    m_fitProblems.setLength( 0 );
    updateProblemsText();
    updateInfoText();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  public void finish()
  {
    super.finish();
    m_srcFE = null;
    m_targetFE = null;
    m_fitProblems.setLength( 0 );
    updateProblemsText();
    updateInfoText();
  }

  /**
   * @return list of
   * @see RelationType that fit to the selected features
   */
  private List getFitList( Feature fromFE, Feature toFE )
  {
    final List fitList = new ArrayList();
    final IKalypsoTheme activeTheme = getActiveTheme();

    if( fromFE == null || toFE == null || activeTheme == null || !( activeTheme instanceof IKalypsoFeatureTheme ) )
      return fitList;
    final GMLWorkspace workspace = ( (IKalypsoFeatureTheme)activeTheme ).getWorkspace();
    final IRelationType[] relations = m_contentProvider.getCheckedRelations();
    for( int i = 0; i < relations.length; i++ )
    {
      final IRelationType relation = relations[i];
      if( relation.fitsTypes( fromFE.getFeatureType(), toFE.getFeatureType() ) )
      {
        final String fitProblems = relation.getFitProblems( workspace, fromFE, toFE, getModificationMode() == MODE_ADD );
        if( fitProblems == null )
          fitList.add( relation );
        else
        {
          m_fitProblems.append( fitProblems );
        }
      }
    }
    if( fitList.isEmpty() && m_fitProblems.length() == 0 )
      m_fitProblems.append( "Ziel nicht erlaubt" );
    return fitList;
  }

  int getModificationMode()
  {
    return m_modificationMode;
  }

  public void dragged( Point p )
  {
    moved( p );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    super.moved( p );
    if( m_srcFE == null )
      return;
    final JMSelector selector = new JMSelector(  );
    final MapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final GM_Point point = GeometryFactory
        .createGM_Point( p, transform, mapPanel.getMapModell().getCoordinatesSystem() );
    double r = transform.getSourceX( RADIUS ) - transform.getSourceX( 0 );
    final Feature feature = selector.selectNearest( point, r, m_allowedFeatureList, false );
    m_fitProblems.setLength( 0 );
    m_targetFE = null;
    if( m_srcFE == feature )
      m_fitProblems.append( "gleiches Element geht nicht" );
    else
    {
      if( !getFitList( m_srcFE, feature ).isEmpty() )
        m_targetFE = feature;
    }
    updateInfoText();
    updateProblemsText();
  }

  public void leftReleased( Point p )
  {
    perform();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    finish();
  }

  public void paint( Graphics g )
  {
    if( m_srcFE == null || m_targetFE == null )
      return;
    final GM_Object fromGeom = m_srcFE.getDefaultGeometryProperty();
    final GM_Object toGeom = m_targetFE.getDefaultGeometryProperty();
    if( fromGeom == null || toGeom == null )
      return;
    final GM_Point fromCenter = fromGeom.getCentroid();
    final GM_Point toCenter = toGeom.getCentroid();
    final MapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    int x1 = (int)transform.getDestX( fromCenter.getX() );
    int y1 = (int)transform.getDestY( fromCenter.getY() );
    int x2 = (int)transform.getDestX( toCenter.getX() );
    int y2 = (int)transform.getDestY( toCenter.getY() );
    g.drawLine( x1, y1, x2, y2 );
  }

  private FeatureList getallowedFeatureList()
  {
    final List result = new ArrayList();
    final IKalypsoTheme activeTheme = getActiveTheme();
    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null || activeTheme == null || !( activeTheme instanceof IKalypsoFeatureTheme ) )
      return new CascadingFeatureList( (FeatureList[])result.toArray( new FeatureList[result.size()] ) );

    final IKalypsoFeatureTheme activeFeatureTheme = (IKalypsoFeatureTheme)activeTheme;
    final GMLWorkspace workspace = activeFeatureTheme.getWorkspace();
    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
    for( int i = 0; i < allThemes.length; i++ )
    {
      if( allThemes[i] != null && allThemes[i] instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme kalypsoFeatureTheme = (IKalypsoFeatureTheme)allThemes[i];
        if( kalypsoFeatureTheme.getWorkspace() == workspace )
          result.add( ( kalypsoFeatureTheme ).getFeatureList() );
      }
    }
    return new CascadingFeatureList( (FeatureList[])result.toArray( new FeatureList[result.size()] ) );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    super.onModellChange( modellEvent );
    refreshSettings();
  }

  private void refreshSettings()
  {
    m_allowedFeatureList = getallowedFeatureList();
    final TreeViewer viewer = m_viewer;
    if( viewer != null && !viewer.getControl().isDisposed() )
    {
      final IKalypsoTheme activeTheme = getActiveTheme();
      if( viewer.getInput() != activeTheme )
        m_viewer.getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run()
          {
            if( viewer != null && !viewer.getControl().isDisposed() )
              viewer.setInput( activeTheme );
          }
        } );
    }
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform()
  {
    final List fitList = getFitList( m_srcFE, m_targetFE );
    m_topLevel.getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        for( Iterator iter = fitList.iterator(); iter.hasNext(); )
        {
          IRelationType element = (IRelationType)iter.next();
          System.out.println( element.toString() );
        }
        // TODO handle fitList.size()>1 with dialog
        if( fitList.size() < 1 )
          return;
        final IRelationType relation;
        if( fitList.size() == 1 )
        {
          relation = (IRelationType)fitList.get( 0 );
        }
        else
        {
          final IStructuredContentProvider cProvider = new IStructuredContentProvider()
          {
            //        private Object m_input = null;

            public Object[] getElements( Object inputElement )
            {
              if( inputElement instanceof List )
              {
                return ( (List)inputElement ).toArray();
              }
              return null;
            }

            public void dispose()
            {
            //          m_input = null;
            }

            public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
            {
            //          m_input = newInput;
            }
          };

          final ListSelectionDialog dialog = new ListSelectionDialog( m_topLevel.getShell(), fitList, cProvider,
              m_labelProvider, fitList.size() + " Relationen möglich, bitte EINE auswählen" );
          dialog.setInitialSelections( new Object[]
          { fitList.get( 0 ) } );
          dialog.setBlockOnOpen( true );
          boolean correct = false;
          Object[] result = null;
          while( !correct )
          {
            int answer = dialog.open();
            if( answer == Window.CANCEL )
              return;
            result = dialog.getResult();
            correct = result.length == 1;
          }
          relation = (IRelationType)result[0];
        }

        CommandableWorkspace workspace = ( (IKalypsoFeatureTheme)getActiveTheme() ).getWorkspace();
        final ICommand command;
        switch( getModificationMode() )
        {
        case MODE_ADD:
          if( relation instanceof HeavyRelationType )
          {
            final HeavyRelationType heavyRealtion = (HeavyRelationType)relation;

            command = new AddHeavyRelationshipCommand( workspace, m_srcFE, heavyRealtion.getLink1(), heavyRealtion
                .getBodyFT(), heavyRealtion.getLink2(), m_targetFE );
          }
          else
          {
            final RelationType normalRelation = (RelationType)relation;
            command = new AddRelationCommand( workspace, m_srcFE, normalRelation.getLink().getName(), 0, m_targetFE );
          }
          break;
        case MODE_REMOVE:
          if( relation instanceof HeavyRelationType )
          {
            final HeavyRelationType heavyRealtion = (HeavyRelationType)relation;

            FindExistingHeavyRelationsFeatureVisitor visitor = new FindExistingHeavyRelationsFeatureVisitor( workspace,
                heavyRealtion );
            visitor.visit( m_srcFE );
            Feature[] bodyFeatureFor = visitor.getBodyFeatureFor( m_targetFE );
            if( bodyFeatureFor.length > 0 )
              command = new RemoveHeavyRelationCommand( workspace, m_srcFE, heavyRealtion.getLink1().getName(),
                  bodyFeatureFor[0], heavyRealtion.getLink2().getName(), m_targetFE );
            else
              command = null;
          }
          else
          {
            final RelationType normalRelation = (RelationType)relation;
            command = new RemoveRelationCommand( workspace, m_srcFE, normalRelation.getLink().getName(), m_targetFE );
          }
          break;
        default:
          command = null;
        }
        try
        {
          workspace.postCommand( command );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    } );
  }

  private void updateProblemsText()
  {
    if( m_textProblem != null && !m_textProblem.isDisposed() )
    {
      m_textProblem.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          String problems = m_fitProblems.toString();
          if( m_textProblem != null && !m_textProblem.isDisposed() )
          {
            if( problems.length() == 0 )
              m_textProblem.setText( "" );
            else
              m_textProblem.setText( problems );
            m_topLevel.layout();
          }
        }
      } );
    }

  }

  void updateInfoText()
  {
    final String lang = KalypsoGisPlugin.getDefault().getLang();
    final StringBuffer labelBuffer = new StringBuffer();
    final StringBuffer tipBuffer = new StringBuffer();
    labelBuffer.append( "Relation" );
    labelBuffer.append( "\n  von: " );
    tipBuffer.append( "Relation" );
    tipBuffer.append( "\n  von: " );

    if( m_srcFE == null )
    {
      labelBuffer.append( "<select>" );
      tipBuffer.append( "<select>" );
    }
    else
    {
      final FeatureType ft = m_srcFE.getFeatureType();
      final Annotation annotation = ft.getAnnotation( lang );
      labelBuffer.append( annotation.getLabel() + "#" + m_srcFE.getId() );
      tipBuffer.append( ft.getNamespace() + ":" + ft.getName() + "#" + m_srcFE.getId() );
    }
    labelBuffer.append( "\n nach: " );
    tipBuffer.append( "\n nach: " );
    if( m_targetFE == null )
    {
      labelBuffer.append( "<select>" );
      tipBuffer.append( "<select>" );
    }
    else
    {
      final FeatureType ft = m_targetFE.getFeatureType();
      final Annotation annotation = ft.getAnnotation( lang );
      labelBuffer.append( annotation.getLabel() + "#" + m_targetFE.getId() );
      tipBuffer.append( ft.getNamespace() + ":" + ft.getName() + "#" + m_targetFE.getId() );
    }
    if( m_textInfo != null && !m_textInfo.isDisposed() )
    {
      m_textInfo.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          if( m_textInfo != null && !m_textInfo.isDisposed() )
          {
            m_textInfo.setText( labelBuffer.toString() );
            m_textInfo.setToolTipText( tipBuffer.toString() );
            m_topLevel.layout();
          }
        }
      } );
    }
    // Andreas: das hatte keine Auswirkungen (mehr). Weg?
    //    if( m_srcFE == null )
    //    {
    //      setLeftMFunction( "Quelle wählen" );
    //      setRightMFunction( null );
    //    }
    //    // src != null && m_targetFE==null
    //    else if( m_targetFE == null )
    //    {
    //      setLeftMFunction( "Ziel wählen" );
    //      setRightMFunction( "Auswahl aufheben" );
    //    }
    //    // src != null && m_targetFE!=null
    //    else
    //    {
    //      setLeftMFunction( "Relation anlegen" );
    //      setRightMFunction( "Auswahl aufheben" );
    //    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl()
  {
    if( m_modeCombo != null && !m_modeCombo.isDisposed() )
      m_modeCombo.dispose();
    if( m_topLevel != null && !m_topLevel.isDisposed() )
      m_topLevel.dispose();
    if( m_viewer != null && !m_viewer.getControl().isDisposed() )
    {
      m_viewer.getControl().dispose();
      m_viewer = null;
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_topLevel = new Composite( parent, SWT.NONE );
    Layout gridLayout = new GridLayout( 1, false );

    m_topLevel.setLayout( gridLayout );
    GridData data = new GridData();
    data.horizontalAlignment = GridData.FILL;
    data.verticalAlignment = GridData.FILL;
    data.grabExcessHorizontalSpace = true;
    data.grabExcessVerticalSpace = true;
    m_topLevel.setLayoutData( data );

    // combo, "add relation" or "remove relation"
    final GridData data3 = new GridData();
    data3.horizontalAlignment = GridData.FILL;
    data3.verticalAlignment = GridData.FILL;
    data3.grabExcessHorizontalSpace = true;
    data3.grabExcessVerticalSpace = false;

    m_modeCombo = new Combo( parent, SWT.READ_ONLY );
    m_modeCombo.setItems( m_modeItems );
    m_modeCombo.setText( m_modeItems[m_modificationMode] );
    m_modeCombo.setLayoutData( data3 );
    m_modeCombo.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        m_modificationMode = ( (Combo)e.getSource() ).getSelectionIndex();
        m_srcFE = null;
        m_targetFE = null;
        updateInfoText();
      }
    } );
    // tree
    final GridData data2 = new GridData();
    data2.horizontalAlignment = GridData.FILL;
    data2.verticalAlignment = GridData.FILL;
    data2.grabExcessHorizontalSpace = true;
    data2.grabExcessVerticalSpace = true;

    //  m_viewer = new CheckboxTreeViewer( parent, SWT.FILL );
    final TreeViewer viewer = new TreeViewer( m_topLevel, SWT.FILL );
    m_viewer = viewer;
    viewer.getControl().setLayoutData( data2 );
    viewer.setContentProvider( m_contentProvider );
    m_labelProvider = new EditRelationOptionsLabelProvider( m_contentProvider );
    viewer.setLabelProvider( m_labelProvider );

    m_textInfo = new Text( m_topLevel, SWT.READ_ONLY | SWT.MULTI | SWT.BORDER | SWT.WRAP );
    m_textInfo.setText( "Info" );

    m_textProblem = new Text( m_topLevel, SWT.READ_ONLY | SWT.MULTI | SWT.WRAP );
    m_textProblem.setText( "Problem" );
    viewer.setAutoExpandLevel( 2 );
    viewer.getTree().addMouseListener( new MouseAdapter()
    {
      public void mouseUp( final MouseEvent e )
      {
        final TreeItem item = viewer.getTree().getItem( new org.eclipse.swt.graphics.Point( e.x, e.y ) );
        if( item != null )
        {
          final Object element = item.getData();
          if( element != null )
          {
            boolean status = m_contentProvider.isChecked( element );
            m_contentProvider.accept( element, new SetCheckedTreeVisitor( !status ) );
            viewer.refresh( element, true );
          }
        }
      }
    } );
    refreshSettings();
  }
}