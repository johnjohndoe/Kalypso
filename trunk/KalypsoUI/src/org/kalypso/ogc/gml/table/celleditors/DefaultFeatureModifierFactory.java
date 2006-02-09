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
package org.kalypso.ogc.gml.table.celleditors;

import java.util.Date;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.IFeatureModifier;
import org.kalypso.ogc.gml.featureview.modfier.BooleanModifier;
import org.kalypso.ogc.gml.featureview.modfier.ButtonModifier;
import org.kalypso.ogc.gml.featureview.modfier.StringModifier;
import org.kalypso.ogc.gml.gui.GuiTypeRegistrySingleton;
import org.kalypso.ogc.gml.gui.IGuiTypeHandler;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Belger
 */
public class DefaultFeatureModifierFactory implements IFeatureModifierFactory
{
  /**
   * @see org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory#createFeatureModifier(org.kalypsodeegree.model.feature.GMLWorkspace,
   *      org.kalypsodeegree.model.feature.IPropertyType, java.lang.String,
   *      org.kalypso.ogc.gml.selection.IFeatureSelectionManager,
   *      org.kalypso.ogc.gml.featureview.IFeatureChangeListener)
   */
  public IFeatureModifier createFeatureModifier( final GMLWorkspace workspace, final IPropertyType ftp, final String format, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl )
  {
    if( ftp instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final Class valueClass = (vpt).getValueClass();

      if( String.class == valueClass )
        return new StringModifier( vpt );
      if( Integer.class == valueClass )
        return new StringModifier( vpt );
      if( Long.class == valueClass )
        return new StringModifier( vpt );
      if( Float.class == valueClass )
        return new StringModifier( vpt );
      if( Double.class == valueClass )
        return new StringModifier( vpt );
      if( Date.class == valueClass )
        return new StringModifier( vpt );
      if( Boolean.class == valueClass )
        return new BooleanModifier( vpt );
      if( vpt.isGeometry() )
        return new ButtonModifier( workspace, vpt, selectionManager, fcl );

      final IGuiTypeHandler typeHandler = (IGuiTypeHandler) GuiTypeRegistrySingleton.getTypeRegistry().getTypeHandlerForClassName( valueClass );
      if( typeHandler != null )
        return typeHandler.createFeatureModifier( workspace, ftp, selectionManager, fcl );
      return new StringModifier( vpt );
    }

    if( ftp instanceof IRelationType )
    {
      IRelationType rpt = (IRelationType) ftp;
      return new ButtonModifier( workspace, rpt, selectionManager, fcl );
    }
    throw new UnsupportedOperationException();
  }
}