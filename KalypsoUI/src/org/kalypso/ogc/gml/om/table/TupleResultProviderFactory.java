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
package org.kalypso.ogc.gml.om.table;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.gmlschema.adapter.AnnotationAdapterFactory;
import org.kalypso.gmlschema.adapter.ILanguageAnnontationProvider;
import org.kalypso.gmlschema.property.restriction.EnumerationRestriction;
import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

/**
 * @author kuch
 */
public class TupleResultProviderFactory
{

  public static CellEditor getCellEditor( final IComponent component, final DefaultTableViewer viewer, final int swtStyle, final boolean isEditable )
  {
    /**
     * Draw combobox, if Component is an enumeration. An enumeration is defined as: <br>
     * (swe:word or xst:string) and enumRestriction <br>
     * <br>
     * otherwise draw an textcelleditor!
     */

    final IRestriction[] restrictions = component.getRestrictions();
    final CellEditor editor;

    if( !isEditable )
    {
      editor = null;
    }
    else if( ComponentUtilities.restrictionContainsEnumeration( restrictions ) )
    {
      final Map<Object, ILanguageAnnontationProvider> items = TupleResultProviderFactory.getEnumerationItems( restrictions );
      final Set<Object> set = items.keySet();

      final LabelProvider labelProvider = new LabelProvider()
      {
        /**
         * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
         */
        @Override
        public String getText( Object element )
        {
          final String lang = AnnotationAdapterFactory.getPlatformLang();
          final ILanguageAnnontationProvider provider = items.get( element );

          if( provider != null )
          {
            return provider.getAnnotation( lang ).getLabel().trim();
          }

          return super.getText( element );
        }
      };
      editor = new ComboBoxViewerCellEditor( new ArrayContentProvider(), labelProvider, set.toArray(), viewer.getTable(), swtStyle | SWT.READ_ONLY | SWT.DROP_DOWN );
    }
    else
    {
      // get an textedit
      editor = new TextCellEditor( viewer.getTable(), swtStyle )
      {
        /**
         * Overwritten in order to allow leaving of the cell-editor after editing via tab
         * 
         * @see org.eclipse.jface.viewers.TextCellEditor#createControl(org.eclipse.swt.widgets.Composite)
         */
        @Override
        protected Control createControl( final Composite parent )
        {
          final Control textControl = super.createControl( parent );
          text.addTraverseListener( new TraverseListener()
          {
            public void keyTraversed( final TraverseEvent e )
            {
              if( (e.detail == SWT.TRAVERSE_TAB_NEXT) || (e.detail == SWT.TRAVERSE_TAB_PREVIOUS) )
              {
                e.doit = false;
              }
            }
          } );

          return textControl;
        }
      };
    }

    return editor;
  }

  private static Map<Object, ILanguageAnnontationProvider> getEnumerationItems( final IRestriction[] restrictions )
  {
    final Map<Object, ILanguageAnnontationProvider> items = new LinkedHashMap<Object, ILanguageAnnontationProvider>();

    for( final IRestriction restriction : restrictions )
    {
      if( restriction instanceof EnumerationRestriction )
      {
        final EnumerationRestriction r = (EnumerationRestriction) restriction;

        items.putAll( r.getMapping() );
      }
    }

    return items;
  }
}
