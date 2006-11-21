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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.swt.custom.ScrolledCompositeCreator;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Use this dialog only once, it cannot be redisplayed.
 * 
 * @author belger
 */
public class FeatureviewDialog extends Dialog
{
  private final FeatureComposite m_featureComposite;

  private static final int APPLY_ID = IDialogConstants.CLIENT_ID + 1;

  private static final int RESET_ID = IDialogConstants.CLIENT_ID + 2;

  private final Collection m_changes = new ArrayList();

  private final GMLWorkspace m_workspace;

  private final ICommandTarget m_target;

  public FeatureviewDialog( final GMLWorkspace workspace, final ICommandTarget target, final Shell parentShell,
      final FeatureComposite featureComposite )
  {
    super( parentShell );
    m_workspace = workspace;
    m_target = target;
    m_featureComposite = featureComposite;

    final Collection changes = m_changes;
    m_featureComposite.addChangeListener( new IFeatureChangeListener()
    {
      public void featureChanged( final FeatureChange change )
      {
        changes.add( change );
        updateButtons();
      }

      public void openFeatureRequested( final Feature feature, final FeatureTypeProperty ftp )
      {
      // TODO: stack dialogs
      }
    } );

    setShellStyle( getShellStyle() | SWT.RESIZE | SWT.MAX );
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
    panel.setLayout( new GridLayout() );
    panel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final FeatureComposite featureComposite = m_featureComposite;
    final ScrolledCompositeCreator creator = new ScrolledCompositeCreator( null )
    {
      protected Control createContents( final Composite scrollParent, int style )
      {
        return (Composite)featureComposite.createControl( scrollParent, style );
      }
    };
    creator.createControl( panel, SWT.V_SCROLL, SWT.NONE );
    creator.getScrolledComposite().setLayoutData( new GridData( GridData.FILL_BOTH ) );

    return creator.getScrolledComposite();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
   */
  protected void createButtonsForButtonBar( Composite parent )
  {
    super.createButtonsForButtonBar( parent );

    createButton( parent, APPLY_ID, "Übernehmen", false );
    createButton( parent, RESET_ID, "Zurücksetzen", false );

    updateButtons();
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
    final FeatureChange[] changesArray = (FeatureChange[])m_changes.toArray( new FeatureChange[m_changes.size()] );
    m_changes.clear();

    if( m_workspace != null )
      m_target.postCommand( new ChangeFeaturesCommand( m_workspace, changesArray ), null );

    updateButtons();
  }

  private void resetPressed()
  {
    m_changes.clear();
    m_featureComposite.updateControl();
    updateButtons();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  protected void okPressed()
  {
    applyPressed();

    m_featureComposite.dispose();

    super.okPressed();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
   */
  protected void cancelPressed()
  {
    m_featureComposite.dispose();

    super.cancelPressed();
  }

  protected void updateButtons()
  {
    final boolean enable = m_changes.size() != 0;
    getButton( IDialogConstants.OK_ID ).setEnabled( enable );
    getButton( APPLY_ID ).setEnabled( enable );
    getButton( RESET_ID ).setEnabled( enable );
  }
}