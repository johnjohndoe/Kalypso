package org.kalypso.model.wspm.tuhh.ui.export.csv;

import java.util.Comparator;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.contribs.java.lang.NumberUtils;

/**
 * @author Gernot Belger
 *
 */
public final class DialogSettingsByNameComparator implements Comparator<IDialogSettings>
{
  @Override
  public int compare( final IDialogSettings o1, final IDialogSettings o2 )
  {
    final String name1 = o1.getName();
    final String name2 = o2.getName();

    final Integer i1 = NumberUtils.parseQuietInteger( name1 );
    final Integer i2 = NumberUtils.parseQuietInteger( name2 );

    if( i1 == null )
      return -1;
    if( i2 == null )
      return 1;

    return i1 - i2;
  }
}