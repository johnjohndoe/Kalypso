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
import java.util.Iterator;
import java.util.LinkedList;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.JumpToFeatureDialog;
import org.kalypso.ogc.gml.featureview.dialog.NotImplementedFeatureDialog;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * This control behaves in two ways: if the type of the proeprty to edit is simple, it opens a fitting dialog to edit
 * the property. If the property is a feature, it just informs its listeners, that it whichs to open that feature.
 * 
 * @author belger
 */
public class ButtonFeatureControl extends AbstractFeatureControl implements ModellEventListener
{
  private Button m_button;

  private IFeatureDialog m_dialog = null;

  private Collection<ModifyListener> m_modifyListener = new ArrayList<ModifyListener>();

  public ButtonFeatureControl( final Feature feature, final IPropertyType ftp )
  {
    super( feature, ftp );

    // fake listener, which just informs my own listeners
    final IFeatureChangeListener listener = new IFeatureChangeListener()
    {
      public void featureChanged( final FeatureChange change )
      {
        fireFeatureChange( change );
      }

      public void openFeatureRequested( final Feature featureToOpen, final IPropertyType ftpToOpen )
      {
        fireOpenFeatureRequested( featureToOpen, ftpToOpen );
      }
    };

    m_dialog = chooseDialog( feature, ftp, listener );
  }

  public static IFeatureDialog chooseDialog( final Feature feature, final IPropertyType ftp, final IFeatureChangeListener listener )
  {
    // final String typename = ftp.getType();
    if( ftp instanceof IValuePropertyType )
    {
      final ITypeRegistry<IGuiTypeHandler> typeRegistry = GuiTypeRegistrySingleton.getTypeRegistry();
      final IGuiTypeHandler handler = typeRegistry.getTypeHandlerFor( ftp );
      if( handler != null )
        return handler.createFeatureDialog( feature, ftp );
    }

    // TODO: make gui type handler for this?
    if( ftp instanceof IRelationType )
    {
      if( ftp.isList() )
      {
        // it is a list of features or links to features or mixed
        // return new FeatureDialog( workspace, feature, ftp, selectionManager );
        return new JumpToFeatureDialog( listener, feature, ftp );
      }
      // it is not a list
      final Object property = feature.getProperty( ftp );
      final Feature linkedFeature;
      if( property instanceof String ) // link auf ein Feature mit FeatureID
      {
        if( ((String) property).length() < 1 )
          return new NotImplementedFeatureDialog( "hier ist kein Element verknüpft", "<leer>" );
        final GMLWorkspace workspace = feature.getWorkspace();
        linkedFeature = workspace.getFeature( (String) property );
      }
      else if( property instanceof Feature )
        linkedFeature = (Feature) property;
      else
        return new NotImplementedFeatureDialog( "hier ist kein Element verknüpft", "<leer>" );

      return new JumpToFeatureDialog( listener, linkedFeature, null );
    }

    return new NotImplementedFeatureDialog();
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
      public void widgetSelected( SelectionEvent e )
      {
        buttonPressed();
      }
    } );

    updateControl();

    return m_button;
  }

  protected void buttonPressed( )
  {
    if( m_dialog.open( m_button.getShell() ) == Window.OK )
    {
      final Collection<FeatureChange> c = new LinkedList<FeatureChange>();
      m_dialog.collectChanges( c );
      for( final Iterator iter = c.iterator(); iter.hasNext(); )
        fireFeatureChange( (FeatureChange) iter.next() );

      fireModfied();

      updateControl();
    }
  }

  private void fireModfied( )
  {
    for( final Iterator iter = m_modifyListener.iterator(); iter.hasNext(); )
    {
      final ModifyListener l = (ModifyListener) iter.next();
      final Event event = new Event();
      event.widget = m_button;
      l.modifyText( new ModifyEvent( event ) );
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
    m_button.setText( m_dialog.getLabel() );
  }
}