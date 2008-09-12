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
package org.kalypso.ui.editor.diagrameditor.actions;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Stroke;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.action.FullAction;
import org.kalypso.ogc.sensor.commands.ChangeThemePropertiesCommand;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewCurve;
import org.kalypso.ogc.sensor.template.ObsViewItem;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.abstractobseditor.ObservationEditorOutlinePage;

/**
 * Allows the user to set the axis-types that should be ignored when displaying observation items
 * 
 * @author schlienger
 */
public class EditDiagCurveAction extends FullAction
{
  private ObservationEditorOutlinePage m_page;

  public EditDiagCurveAction( final ObservationEditorOutlinePage page )
  {
    super( "Eigenschaften", ImageProvider.IMAGE_OBSVIEW_CURVE_PROPERTIES,
        "Erlaubt die Eigenschaften des Themas zu bearbeiten" );

    m_page = page;
  }

  @Override
  public void run()
  {
    final Shell shell = m_page.getSite().getShell();

    final DiagView obsView = (DiagView)m_page.getView();
    final ObsViewItem[] items = m_page.getSelectedItems();
    
    if( items.length == 0 )
    {
      MessageDialog.openWarning(shell, "Kurveneigenschaften", "Bitte wählen Sie eine oder mehrere Kurven in der Gliederungsansicht.");
      return;
    }
    
    final LineProperties[] currentProperties = new LineProperties[items.length];
    for( int i = 0; i < items.length; i++ )
      currentProperties[i] = itemToProperties( (DiagViewCurve)items[i] );

    final LineProperties lineProperties = LineProperties.mergeProperties( currentProperties );

    final EditDiagCurveDialog dialog = new EditDiagCurveDialog( shell, lineProperties )
    {
      /**
       * @see org.kalypso.ui.editor.diagrameditor.actions.EditDiagCurveDialog#propertiesChanged()
       */
      @Override
      protected void propertiesChanged()
      {
        final LineProperties resultProperties = getLineProperties();
        for( int i = 0; i < items.length; i++ )
          applyLineProperties( obsView, (DiagViewCurve)items[i], resultProperties, items.length == 1 );
      }
    };

    if( dialog.open() == Window.CANCEL )
    {
      for( int i = 0; i < items.length; i++ )
        applyLineProperties( obsView, (DiagViewCurve)items[i], currentProperties[i], true );
    }
  }

  private LineProperties itemToProperties( final DiagViewCurve curve )
  {
    final String name = curve.getName();
    final Color color = curve.getColor();
    final Stroke stroke = curve.getStroke();
    final Integer size;
    final DashType dash;

    if( stroke instanceof BasicStroke )
    {
      final BasicStroke basicStroke = (BasicStroke)stroke;
      size = new Integer( (int)basicStroke.getLineWidth() );

      final float[] curveDash = basicStroke.getDashArray();
      final DashType userDash = new DashType( "User defined", "Benutzerdefiniert", curveDash == null ? new float[] {}
          : curveDash );
      if( DashType.isKnownDash( userDash ) )
        dash = DashType.findKnownDash( userDash );
      else
        dash = userDash;
    }
    else
    {
      size = LineProperties.SIZE_UNDEF;
      dash = DashType.NONE;
    }

    return new LineProperties( name, color, size, dash );
  }

  /**
   * Applies the changed properties to all selected items. <br>
   * Only applies changed values (i.e. non- <code>null</code>) We do not always apply the name, as the name is used
   * as id by the diagramm... this causes bugs, if two curves have the same name.
   */
  protected void applyLineProperties( final DiagView view, final DiagViewCurve item, final LineProperties properties,
      final boolean applyName )
  {
    final String nameToSet;
    final Color colorToSet;
    final Stroke strokeToSet;

    final String name = properties.getName();
    if( applyName && name != EditDiagCurveDialog.NAME_UNDEF )
      nameToSet = name;
    else
      nameToSet = item.getName();

    final Color color = properties.getColor();
    if( color == LineProperties.COLOR_UNDEF )
      colorToSet = item.getColor();
    else
      colorToSet = color;

    final Stroke stroke = properties.getStroke();
    if( stroke == LineProperties.STROKE_UNDEF )
      strokeToSet = item.getStroke();
    else
      strokeToSet = stroke;

    final ChangeThemePropertiesCommand command = new ChangeThemePropertiesCommand( view, item, nameToSet, colorToSet,
        strokeToSet );
    m_page.getEditor().postCommand( command, null );
  }

}