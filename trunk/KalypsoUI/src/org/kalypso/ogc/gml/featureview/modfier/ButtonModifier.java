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
package org.kalypso.ogc.gml.featureview.modfier;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.control.ButtonFeatureControl;
import org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.table.celleditors.DialogCellEditor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author belger
 */
public class ButtonModifier implements IFeatureModifier
{
  private final FeatureTypeProperty m_ftp;

  private final GMLWorkspace m_workspace;

  private Feature m_feature;

  private final IFeatureSelectionManager m_selectionManager;

  public ButtonModifier( final GMLWorkspace workspace, final FeatureTypeProperty ftp, final IFeatureSelectionManager selectionManager )
  {
    m_workspace = workspace;
    m_ftp = ftp;
    m_selectionManager = selectionManager;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getValue(org.kalypsodeegree.model.feature.Feature)
   */
  public Object getValue( final Feature f )
  {
    m_feature = f;
    return f.getProperty( m_ftp.getName() );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#parseInput(org.kalypsodeegree.model.feature.Feature,
   *      java.lang.Object)
   */
  public Object parseInput( final Feature f, final Object value )
  {
    return value;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#createCellEditor(org.eclipse.swt.widgets.Composite)
   */
  public CellEditor createCellEditor( final Composite parent )
  {
    final IFeatureSelectionManager manager = m_selectionManager;
    return new DialogCellEditor( parent )
    {
      IFeatureDialog m_featureDialog = null;

      protected boolean openDialog( final Control parentControl )
      {
        // listener will be called, when a linked feature should be edited
        final IFeatureChangeListener listener = new IFeatureChangeListener() {

          public void featureChanged( final FeatureChange change )
          {
          }

          public void openFeatureRequested( final Feature feature )
          {
            // what to do? normally feature should be selected, is this always ok?
          }};
        
        m_featureDialog = ButtonFeatureControl.chooseDialog( getWorkspace(), getFeature(), getFeatureTypeProperty(), listener, manager );
        return m_featureDialog.open( parentControl.getShell() ) == Window.OK;
      }

      /**
       * @see org.kalypso.ogc.gml.table.celleditors.DialogCellEditor#doGetValue()
       */
      protected Object doGetValue()
      {
        // collect changes from dialog
        final List col = new ArrayList();
        m_featureDialog.collectChanges( col );
        if( col.size() > 1 ) // TODO support more
          throw new UnsupportedOperationException( "Dialog must provide exactly one change" );
        if( col.size() > 0 )
        {
          final Object change = col.get( 0 );
          if( change instanceof FeatureChange )
            return change;
        }
        return super.doGetValue();
      }
    };
  }

  protected GMLWorkspace getWorkspace()
  {
    return m_workspace;
  }

  protected Feature getFeature()
  {
    return m_feature;
  }

  /**
   * @see org.eclipse.jface.viewers.ICellEditorValidator#isValid(java.lang.Object)
   */
  public String isValid( final Object value )
  {
    return null; // null means vaild
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getFeatureTypeProperty()
   */
  public FeatureTypeProperty getFeatureTypeProperty()
  {
    return m_ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getLabel(org.kalypsodeegree.model.feature.Feature)
   */
  public String getLabel( final Feature f )
  {
    // besser: abh�ngig von der FeatureTypeProperty etwas machen
    final Object value = getValue( f );
    final IGuiTypeHandler handler = (IGuiTypeHandler)GuiTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForClassName( m_ftp.getType() );
    if( handler != null && value != null )
      return handler.getText(value);

    return "<Editieren...>";
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#getImage(org.kalypsodeegree.model.feature.Feature)
   */
  public Image getImage( final Feature f )
  {
    // Todo: button image
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#dispose()
   */
  public void dispose()
  {
  // nichts zu tun
  }
  
  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureModifier#equals(java.lang.Object, java.lang.Object)
   */
  public boolean equals( final Object newData, final Object oldData )
  {
    return newData.equals( oldData );
  }
}