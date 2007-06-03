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
package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IAction;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.i18n.Messages;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.osgi.framework.Bundle;

/**
 * This feature control displays a button which runs an arbitrary action.
 * 
 * @author Gernot Belger
 */
public class ActionButtonFeatureControl extends AbstractFeatureControl implements ModellEventListener
{
  private Button m_button;

  private final Collection<ModifyListener> m_modifyListener = new ArrayList<ModifyListener>();

  private final IAction m_action;

  /**
   * @param pluginId
   *            The plugin from which the action will be loaded.
   * @param actionClass
   *            The classname of the action to load. The class must implement {@link org.eclipse.jface.action.IAction}
   */
  public ActionButtonFeatureControl( final Feature feature, final IPropertyType ftp, final String pluginId, final String actionClass )
  {
    super( feature, ftp );

    m_action = loadAction( pluginId, actionClass );
  }

  private static IAction loadAction( final String pluginId, final String actionClassName )
  {
    final ILog log = KalypsoGisPlugin.getDefault().getLog();

    final Bundle bundle = Platform.getBundle( pluginId );
    if( bundle == null )
    {
      log.log( StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.ogc.gml.featureview.control.ActionButtonFeatureControl.idnotfound" ) + pluginId ) ); //$NON-NLS-1$
      return null;
    }

    try
    {
      final Class< ? > actionClass = bundle.loadClass( actionClassName );
      return (IAction) actionClass.newInstance();
    }
    catch( final ClassNotFoundException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.ogc.gml.featureview.control.ActionButtonFeatureControl.classnotfound" ) + actionClassName, e ); //$NON-NLS-1$
      log.log( status );
    }
    catch( final InstantiationException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.ogc.gml.featureview.control.ActionButtonFeatureControl.instantiate" ) + actionClassName, e ); //$NON-NLS-1$
      log.log( status );
    }
    catch( final IllegalAccessException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.ogc.gml.featureview.control.ActionButtonFeatureControl.access" ) + actionClassName, e ); //$NON-NLS-1$
      log.log( status );
    }
    catch( final ClassCastException e )
    {
      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.ogc.gml.featureview.control.ActionButtonFeatureControl.implement" ) + actionClassName, e ); //$NON-NLS-1$
      log.log( status );
    }

    return null;
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  @Override
  public void dispose( )
  {
    if( !(m_button.isDisposed()) )
      m_button.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_button = new Button( parent, style );
    m_button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        buttonPressed( e );
      }
    } );

    if( m_action == null )
    {
      m_button.setText( "-" //$NON-NLS-1$
          + Messages.getString( "org.kalypso.ogc.gml.featureview.control.ActionButtonFeatureControl.invalid" ) //$NON-NLS-1$ 
          + "-" ); //$NON-NLS-1$
      m_button.setImage( null );
      m_button.setToolTipText( null );
    }
    else
    {
      m_button.setText( m_action.getText() );
      // m_button.setImage( m_action.getImageDescriptor() );
      m_button.setToolTipText( m_action.getToolTipText() );
    }

    updateControl();

    return m_button;
  }

  protected void buttonPressed( final SelectionEvent e2 )
  {
    // todo action
    if( m_action != null )
    {
      // Run with event in order to provide a shell for the action
      final Event e = new Event();
      e.data = e2.data;
      e.detail = e2.detail;
      e.display = e2.display;
      e.doit = e2.doit;
      e.height = e2.height;
      e.item = e2.item;
      e.stateMask = e2.stateMask;
      e.text = e2.text;
      e.time = e2.time;
      e.widget = e2.widget;
      e.width = e2.width;
      e.x = e2.x;
      e.y = e2.y;

      m_action.runWithEvent( e );
    }
  }

  /**
   * Die ButtonControl ist immer valid
   * 
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_modifyListener.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_modifyListener.remove( l );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    updateControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    // Action is fixed, so nothing to do except enabling/disabling the button

  }
}