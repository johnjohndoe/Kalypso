package org.kalypso.eclipse.jface.viewers;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.util.factory.FactoryException;

/**
 * @author gernot
 */
public interface ICellEditorFactory
{
  public CellEditor createEditor( final FeatureTypeProperty ftp, final Composite parent, final int style ) throws FactoryException;
}
