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
package org.kalypso.ogc.gml.featureview;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class FeatureviewDialog extends Dialog implements ModifyListener
{
  private final FeatureComposite m_featureComposite;

  private static final int APPLY_ID = IDialogConstants.CLIENT_ID + 1;

  private static final int RESET_ID = IDialogConstants.CLIENT_ID + 2;

  private final Collection m_changes = new ArrayList();

  private final GMLWorkspace m_workspace;

  private final ICommandTarget m_target;

  public FeatureviewDialog( final GMLWorkspace workspace, final ICommandTarget target,
      final Shell parentShell, final FeatureComposite featureComposite )
  {
    super( parentShell );
    m_workspace = workspace;
    m_target = target;
    m_featureComposite = featureComposite;
    
    setShellStyle( getShellStyle() | SWT.RESIZE );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    getShell().setText( "Feature editieren" );

    final Feature feature = m_featureComposite.getFeature();
    final FeatureType featureType = feature.getFeatureType();
    
    final Group panel = new Group( parent, SWT.NONE );
    panel.setText( featureType.getName() + " - " + feature.getId() );
    panel.setLayout( new GridLayout( ) );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    
    final ScrolledComposite scrolledComposite = new ScrolledComposite( panel, SWT.H_SCROLL
        | SWT.V_SCROLL );

    // don't forget that line!
    scrolledComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Composite control = (Composite)m_featureComposite.createControl( scrolledComposite,
        SWT.NONE );
    scrolledComposite.setContent( control );

   // updateControlSize( scrolledComposite, control );

    panel.addControlListener( new ControlAdapter()
    {
      /**
       * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
       */
      public void controlResized( final ControlEvent e )
      {
        updateControlSize( scrolledComposite, control );
      }
    } );

    m_featureComposite.addModifyListener( this );

    return scrolledComposite;
  }

  protected void updateControlSize( final ScrolledComposite scrolledComposite, final Composite control )
  {
    final Point controlSize = control.computeSize( SWT.DEFAULT, SWT.DEFAULT );
    final Rectangle clientArea = scrolledComposite.getClientArea();
    final int psizex = clientArea.width;
    final int psizey = clientArea.height;
    
    final Point newSize = new Point( Math.max( controlSize.x, psizex ), Math.max( controlSize.y, psizey ) );
    control.setSize( newSize );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  protected void createButtonsForButtonBar( Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    createButton( parent, APPLY_ID, "Übernehmen", false );
    createButton( parent, RESET_ID, "Zurücksetzen", false );

    updateButtons( false );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
   */
  protected void buttonPressed( int buttonId )
  {
    switch( buttonId )
    {
    case APPLY_ID:
      applyPressed();
      break;

    case RESET_ID:
      resetPressed();
      break;

    default:
      super.buttonPressed( buttonId );
    }
  }

  public void collectChanges( final Collection c )
  {
    c.addAll( m_changes );
  }

  private void applyPressed()
  {
    m_changes.clear();

    m_featureComposite.collectChanges( m_changes );

    if( m_workspace != null )
      m_target.postCommand( new ChangeFeaturesCommand( m_workspace, (FeatureChange[])m_changes
          .toArray( new FeatureChange[m_changes.size()] ) ), null );

    updateButtons( false );
  }

  private void resetPressed()
  {
    m_featureComposite.updateControl();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed()
  {
    applyPressed();

    m_featureComposite.removeModifyListener( this );

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  protected void cancelPressed()
  {
    m_featureComposite.removeModifyListener( this );

    super.cancelPressed();
  }

  private void updateButtons( final boolean bEnable )
  {
    getButton( IDialogConstants.OK_ID ).setEnabled( bEnable );
    getButton( APPLY_ID ).setEnabled( bEnable );
    getButton( RESET_ID ).setEnabled( bEnable );
  }

  /**
   * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
   */
  public void modifyText( ModifyEvent e )
  {
    final Collection changes = new ArrayList();
    m_featureComposite.collectChanges( changes );

    final boolean bDirty = changes.size() != 0;

    updateButtons( bDirty );
  }
}