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
package org.kalypso.kalypsomodel1d2d.ui.featurecontrols;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Dejan Antanaskovic
 */
public class ControlModelDeleteControl extends AbstractFeatureControl implements IFeatureControl
{
  private Button m_button;

  public ControlModelDeleteControl( Feature feature, IPropertyType ftp )
  {
    super( feature, ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  @Override
  public void addModifyListener( ModifyListener l )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  public Control createControl( final Composite parent, final int style )
  {
    m_button = new Button( parent, style );

    m_button.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ControlModelDeleteControl.0") ); //$NON-NLS-1$

    m_button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        if( MessageDialog.openConfirm( parent.getShell(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ControlModelDeleteControl.1"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ControlModelDeleteControl.2") ) ) //$NON-NLS-1$ //$NON-NLS-2$
        {
          final Feature parentFeature = getFeature();
          final Object property = parentFeature.getProperty( getFeatureTypeProperty() );
          if( property instanceof XLinkedFeature_Impl )
          {
            final CommandableWorkspace commandableWorkspace = new CommandableWorkspace( parentFeature.getWorkspace() );
            final Feature fLinked = ((XLinkedFeature_Impl) property).getFeature();

            /**
             * XLinkedFeature_Impl's method getFeature will not return the instance from the collection (GM_envelope is
             * null), so it cannot be removed by DeleteFeatureCommand; That's why we are getting feature from
             * CommandableWorkspace
             */

            final Feature f = commandableWorkspace.getFeature( fLinked.getId() );
            final DeleteFeatureCommand command = new DeleteFeatureCommand( f );
            try
            {
              commandableWorkspace.postCommand( command );
              // commandManager.postCommand( command );
            }
            catch( Exception e1 )
            {
              // TODO Auto-generated catch block
              e1.printStackTrace();
            }
          }
        }
        // for( FeatureChange change : changes );
        // fireFeatureChange( change );
      }
    } );
    return m_button;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  @Override
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  @Override
  public void removeModifyListener( ModifyListener l )
  {
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  @Override
  public void updateControl( )
  {
  }

}
