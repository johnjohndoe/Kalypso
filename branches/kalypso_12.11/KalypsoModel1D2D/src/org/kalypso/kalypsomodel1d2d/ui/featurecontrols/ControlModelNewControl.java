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

import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IXLinkedFeature;

/**
 * @author Dejan Antanaskovic
 */
public class ControlModelNewControl extends AbstractFeatureControl
{
  private Button m_button;

  public ControlModelNewControl( final Feature feature, final IPropertyType ftp )
  {
    super( feature, ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  @Override
  public void addModifyListener( final ModifyListener l )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  public Control createControl( final Composite parent, final int style )
  {
    m_button = new Button( parent, style );

    m_button.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.ControlModelNewControl.0" ) ); //$NON-NLS-1$

    m_button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final Feature parentFeature = getFeature();
        final Object property = parentFeature.getProperty( getFeatureTypeProperty() );
        if( property instanceof IXLinkedFeature )
        {
          final Feature f = (IXLinkedFeature) property;
          final IRelationType relationType = f.getParentRelation();
          final CommandableWorkspace commandableWorkspace = new CommandableWorkspace( parentFeature.getWorkspace() );
          final int pos = 0;
          final AddFeatureCommand command = new AddFeatureCommand( commandableWorkspace, f.getFeatureType(), parentFeature, relationType, pos, null, null, -1 );
          try
          {
            commandableWorkspace.postCommand( command );
            // KalypsoGisPlugin.getDefault().getPool().saveObject( parentFeature.getWorkspace(), new
            // NullProgressMonitor() );
            // final FeatureChange change = new FeatureChange(commandableWorkspace.getRootFeature(),
            // parentFeature.getParentRelation(), parentFeature);
            // final FeatureChange[] changes = new FeatureChange[] {change};
            // fireFeatureChange( changes );
          }
          catch( final Exception e1 )
          {
            // TODO Auto-generated catch block
            e1.printStackTrace();
          }
        }
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
  public void removeModifyListener( final ModifyListener l )
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
