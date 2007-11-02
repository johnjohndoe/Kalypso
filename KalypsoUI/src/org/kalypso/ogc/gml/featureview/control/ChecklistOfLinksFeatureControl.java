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
package org.kalypso.ogc.gml.featureview.control;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.internal.WorkbenchMessages;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2;
import org.kalypso.ui.editor.gmleditor.util.command.AddRelationCommand;
import org.kalypso.ui.editor.gmleditor.util.command.RemoveRelationCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Support the following parameters:
 * <ul>
 * <li>showSelectButtons : boolean - if true, 'selectAll' and 'deselecAll' buttons are shown
 * </ul>
 * 
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class ChecklistOfLinksFeatureControl extends AbstractFeatureControl implements IFeatureControl
{
  public static final String PARAM_SELECT_BUTTONS = "showSelectButtons";

  private final Set<ModifyListener> m_modifyListeners = new HashSet<ModifyListener>();

  private final boolean m_showSelectButtons;

  private CheckboxTableViewer m_linkChecklist;

  public ChecklistOfLinksFeatureControl( final Feature feature, final IPropertyType ftp, final boolean showSelectButtons )
  {
    super( feature, ftp );
    m_showSelectButtons = showSelectButtons;
  }

  public ChecklistOfLinksFeatureControl( final IPropertyType pt, final boolean showSelectButtons )
  {
    super( pt );
    m_showSelectButtons = showSelectButtons;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_modifyListeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_modifyListeners.remove( l );
  }

  /**
   * @return Always <code>true</code>
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    // can never be invalid
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    final GC gc = new GC( parent );
    gc.setFont( JFaceResources.getDialogFont() );
    final FontMetrics fontMetrics = gc.getFontMetrics();
    gc.dispose();

    final Composite panel = new Composite( parent, style );
    final GridLayout panelLayout = new GridLayout( 1, false );
    panelLayout.marginWidth = 0;
    panelLayout.marginHeight = 0;
    panel.setLayout( panelLayout );

    m_linkChecklist = CheckboxTableViewer.newCheckList( panel, SWT.BORDER );
    m_linkChecklist.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_linkChecklist.setContentProvider( new ArrayContentProvider() );
    m_linkChecklist.setLabelProvider( new GMLEditorLabelProvider2() );

    // TODO: we never show the buttons, as they do not work properly yet
    if( m_showSelectButtons && false )
    {
      final Composite buttonPanel = createSelectButtons( fontMetrics, panel, m_linkChecklist );
      buttonPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    }

    m_linkChecklist.addCheckStateListener( new ICheckStateListener()
    {
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        handleFeatureChecked( event );
      }
    } );

    updateControl();

    return panel;
  }

  private Composite createSelectButtons( final FontMetrics fontMetrics, final Composite panel, final CheckboxTableViewer checkList )
  {
    final Composite buttonPanel = new Composite( panel, SWT.NONE );
    final GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.marginWidth = 0;
    layout.horizontalSpacing = Dialog.convertHorizontalDLUsToPixels( fontMetrics, IDialogConstants.HORIZONTAL_SPACING );
    buttonPanel.setLayout( layout );

    /* Glue, to force butons to the right */
    final Label label = new Label( buttonPanel, SWT.NONE );
    label.setLayoutData( new GridData( SWT.FILL, SWT.LEFT, true, false ) );

    // TODO: the stuff below does not work yet, as the modell is not changed....

    final Button selectButton = createButton( buttonPanel, IDialogConstants.SELECT_ALL_ID, WorkbenchMessages.SelectionDialog_selectLabel, fontMetrics );
    selectButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        checkList.setAllChecked( true );
      }
    } );

    final Button deselectButton = createButton( buttonPanel, IDialogConstants.DESELECT_ALL_ID, WorkbenchMessages.SelectionDialog_deselectLabel, fontMetrics );
    deselectButton.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        checkList.setAllChecked( false );
      }
    } );

    return buttonPanel;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    /* Set all referenceable features as input */
    final Feature feature = getFeature();
    final IRelationType rt = (IRelationType) getFeatureTypeProperty();
    final GMLWorkspace workspace = feature.getWorkspace();

    final Feature[] features = ComboFeatureControl.collectReferencableFeatures( workspace, feature, rt );
    m_linkChecklist.setInput( features );

    /* check all currently set links */
    final FeatureList linkList = (FeatureList) feature.getProperty( rt );
    for( final Object object : linkList )
    {
      final Feature linkedFeature = FeatureHelper.getFeature( workspace, object );
      m_linkChecklist.setChecked( linkedFeature, true );
    }
  }

  /**
   * REMARK: COPIED FROM {@link Dialog}.
   * <p>
   * Creates a new button with the given id.
   * </p>
   * <p>
   * The <code>Dialog</code> implementation of this framework method creates a standard push button, registers it for
   * selection events including button presses, and registers default buttons with its shell. The button id is stored as
   * the button's client data. If the button id is <code>IDialogConstants.CANCEL_ID</code>, the new button will be
   * accessible from <code>getCancelButton()</code>. If the button id is <code>IDialogConstants.OK_ID</code>, the
   * new button will be accesible from <code>getOKButton()</code>. Note that the parent's layout is assumed to be a
   * <code>GridLayout</code> and the number of columns in this layout is incremented. Subclasses may override.
   * </p>
   * 
   * @param parent
   *            the parent composite
   * @param id
   *            the id of the button (see <code>IDialogConstants.*_ID</code> constants for standard dialog button ids)
   * @param label
   *            the label from the button
   * @param defaultButton
   *            <code>true</code> if the button is to be the default button, and <code>false</code> otherwise
   * @return the new button
   * @see #getCancelButton
   * @see #getOKButton()
   */
  protected Button createButton( final Composite parent, final int id, final String label, final FontMetrics fontMetrics )
  {
    // increment the number of columns in the button bar
    ((GridLayout) parent.getLayout()).numColumns++;
    final Button button = new Button( parent, SWT.PUSH );
    button.setText( label );
    button.setFont( JFaceResources.getDialogFont() );
    button.setData( new Integer( id ) );

    final GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    final int widthHint = Dialog.convertHorizontalDLUsToPixels( fontMetrics, IDialogConstants.BUTTON_WIDTH );
    final Point minSize = button.computeSize( SWT.DEFAULT, SWT.DEFAULT, true );
    data.widthHint = Math.max( widthHint, minSize.x );
    button.setLayoutData( data );

    return button;
  }

  protected void handleFeatureChecked( final CheckStateChangedEvent event )
  {
    final Feature feature = getFeature();
    final IRelationType rt = (IRelationType) getFeatureTypeProperty();
    final FeatureList listOfLinks = (FeatureList) feature.getProperty( rt );

    final Object checkedElement = event.getElement();
    final CompositeCommand compositeCommand = new CompositeCommand( "Linkliste ver‰ndern" );
    if( event.getChecked() )
    {
      // TODO: implement case for external features
      if( checkedElement instanceof Feature )
      {
        compositeCommand.addCommand( new AddRelationCommand( feature, rt, -1, (Feature) checkedElement ) );
      }
    }
    else
    {
      for( final Object object : listOfLinks )
      {
        // TODO: implement case for external features
        // TODO: potential performance problem
        if( object instanceof String && checkedElement instanceof Feature )
        {
          if( object.equals( ((Feature) checkedElement).getId() ) )
            compositeCommand.addCommand( new RemoveRelationCommand( feature, rt, (Feature) checkedElement ) );
        }
      }
    }

    final ModifyListener[] listeners = m_modifyListeners.toArray( new ModifyListener[m_modifyListeners.size()] );

    final Event e = new Event();
    e.widget = m_linkChecklist.getControl();
    // do we need any other properties of e here?
    final ModifyEvent me = new ModifyEvent( e );
    for( final ModifyListener modifyListener : listeners )
      modifyListener.modifyText( me );

    fireFeatureChange( compositeCommand );
  }
}
