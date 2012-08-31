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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.jface.action.ActionButton;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.contribs.eclipse.swt.widgets.ReflowExpansionListener;
import org.kalypso.contribs.eclipse.ui.forms.ToolkitUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */
class ApplyElevationWidgetFace
{
  private TableViewer m_nodeElevationViewer;

  static private IPreferenceStore preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private final IPropertyChangeListener storePropertyChangeListener = new IPropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent event )
    {
      handlePropertyChange( event );
    }
  };

  final ApplyElevationWidgetDataModel m_dataModel;

  private final IFeatureSelectionListener featureSelectionListener = new IFeatureSelectionListener()
  {
    @Override
    public void selectionChanged( final Object source, final IFeatureSelection selection )
    {
      handleSelectionChanged( selection );
    }
  };

  public ApplyElevationWidgetFace( final ApplyElevationWidgetDataModel dataModel )
  {
    m_dataModel = dataModel;
  }

  protected void handlePropertyChange( final PropertyChangeEvent event )
  {
    final Object source = event.getSource();

    if( source instanceof FieldEditor )
    {
      ((FieldEditor) source).store();
    }
    else if( source instanceof ColorSelector )
    {
    }
    else
    {
      System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
  }

  public Control createControl( final Composite parent )
  {
    m_dataModel.getMapPanel().getSelectionManager().addSelectionListener( featureSelectionListener );
    preferenceStore.addPropertyChangeListener( storePropertyChangeListener );

    final Composite rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );

    final FormToolkit toolkit = ToolkitUtils.createToolkit( rootPanel );

    final ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    final Composite body = scrolledForm.getBody();
    GridLayoutFactory.fillDefaults().applyTo( body );

    createSelectElevationModel( toolkit, body ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    createSelectRegion( toolkit, body ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createToolsRegion( toolkit, body ).setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    final Section selectControl = createSelectColor( toolkit, body );
    selectControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    selectControl.addExpansionListener( new ReflowExpansionListener( scrolledForm ) );

    return rootPanel;
  }

  private Control createToolsRegion( final FormToolkit toolkit, final Composite parent )
  {
    final Section region = toolkit.createSection( parent, Section.TITLE_BAR );
    region.setText( Messages.getString( "ApplyElevationWidgetFace.0" ) ); //$NON-NLS-1$

    final Composite panel = toolkit.createComposite( region );
    region.setClient( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    final Action previewAction = new PreviewModelDtm( m_dataModel );

    final Button dtmButton = ActionButton.createButton( toolkit, panel, previewAction );
    dtmButton.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, true, false ) );

    return region;
  }

  // Creates Section to Configure the Color for Different Elevations
  private Section createSelectColor( final FormToolkit toolkit, final Composite parent )
  {
    final Section elevationColorConfig = toolkit.createSection( parent, Section.TITLE_BAR | Section.TWISTIE );
    elevationColorConfig.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidgetFace.4" ) );//$NON-NLS-1$

    final ColorModelChangeComponent colorModelControl = new ColorModelChangeComponent( toolkit, elevationColorConfig, m_dataModel );
    elevationColorConfig.setClient( colorModelControl );

    return elevationColorConfig;
  }

  private final Control createSelectElevationModel( final FormToolkit toolkit, final Composite parent )
  {
    final Section elevationSection = toolkit.createSection( parent, Section.DESCRIPTION | Section.TITLE_BAR );
    elevationSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidgetFace.2" ) ); //$NON-NLS-1$
    elevationSection.setDescription( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.6" ) ); //$NON-NLS-1$
    elevationSection.setLayout( new FillLayout() );

    final ElevationModelSystemEditorComponent editor = new ElevationModelSystemEditorComponent( toolkit, elevationSection, m_dataModel );
    elevationSection.setClient( editor );

    return elevationSection;
  }

  public void disposeControl( )
  {
    preferenceStore.removePropertyChangeListener( storePropertyChangeListener );

    final IMapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel != null )
      mapPanel.getSelectionManager().addSelectionListener( featureSelectionListener );
  }

  // Creates Section for "Select A Region - among the List of Nodes drawn on the Viewer Pane"
  private Control createSelectRegion( final FormToolkit toolkit, final Composite parent )
  {
    final Section configSection = toolkit.createSection( parent, Section.TITLE_BAR );
    configSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidgetFace.3" ) ); //$NON-NLS-1$
    configSection.setLayout( new FillLayout() );

    final AssignNodeElevationFaceComponent editor = new AssignNodeElevationFaceComponent( toolkit, configSection, m_dataModel );
    configSection.setClient( editor );

    // FIXME: the both sections should communicate via the model, what else is it made for?!
    m_nodeElevationViewer = editor.getTableViewer();

    return configSection;
  }

  protected void handleSelectionChanged( final IFeatureSelection selection )
  {
    // FIXME: should go via the data model instead
    final IFE1D2DNode[] nodeList = findSelectedNodes( selection );
    ViewerUtilities.setInput( m_nodeElevationViewer, nodeList, false );
  }

  protected IFE1D2DNode[] findSelectedNodes( final IFeatureSelection selection )
  {
    final List<IFE1D2DNode> nodeList = new ArrayList<>();

    for( final Object selected : selection.toList() )
    {
      Feature selecFeature = null;
      if( selected instanceof Feature )
        selecFeature = (Feature) selected;
      else if( selected instanceof EasyFeatureWrapper )
        selecFeature = ((EasyFeatureWrapper) selected).getFeature();

      if( selecFeature != null )
      {
        final IFE1D2DNode selecNode = (IFE1D2DNode) selecFeature.getAdapter( IFE1D2DNode.class );
        if( selecNode != null )
          nodeList.add( selecNode );
      }
    }

    return nodeList.toArray( new IFE1D2DNode[nodeList.size()] );
  }
}