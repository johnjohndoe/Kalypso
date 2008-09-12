/*
 * Tool Tips example snippet: 
 *
 * For a list of all SWT example snippets see
 * http://dev.eclipse.org/viewcvs/index.cgi/%7Echeckout%7E/platform-swt-home/dev.html#snippets
 */
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

public class Snippet125 {

  public static void main(String[] args) {
    final Display display = new Display();
    final Shell shell = new Shell(display);
    shell.setLayout(new FillLayout());
    final Table table = new Table(shell, SWT.BORDER);
    for (int i = 0; i < 20; i++) {
      TableItem item = new TableItem(table, SWT.NONE);
      item.setText("item " + i);
    }
    // Disable native tooltip
    table.setToolTipText("");

    // Implement a "fake" tooltip
    final Listener labelListener = new Listener() {
      public void handleEvent(Event event) {
        Label label = (Label) event.widget;
        Shell shell = label.getShell();
        switch (event.type) {
        case SWT.MouseDown:
          Event e = new Event();
          e.item = (TableItem) label.getData("_TABLEITEM");
          // Assuming table is single select, set the selection as if
          // the mouse down event went through to the table
          table.setSelection(new TableItem[] { (TableItem) e.item });
          table.notifyListeners(SWT.Selection, e);
        // fall through
        case SWT.MouseExit:
          shell.dispose();
          break;
        }
      }
    };

    Listener tableListener = new Listener() {
      Shell tip = null;

      Label label = null;

      public void handleEvent(Event event) {
        switch (event.type) {
        case SWT.Dispose:
        case SWT.KeyDown:
        case SWT.MouseMove: {
          if (tip == null)
            break;
          tip.dispose();
          tip = null;
          label = null;
          break;
        }
        case SWT.MouseHover: {
          TableItem item = table.getItem(new Point(event.x, event.y));
          if (item != null) {
            if (tip != null && !tip.isDisposed())
              tip.dispose();
            tip = new Shell(shell, SWT.ON_TOP | SWT.TOOL);            tip.setLayout(new FillLayout());
            label = new Label(tip, SWT.NONE);
            label.setForeground(display
                .getSystemColor(SWT.COLOR_INFO_FOREGROUND));
            label.setBackground(display
                .getSystemColor(SWT.COLOR_INFO_BACKGROUND));
            label.setData("_TABLEITEM", item);
            label.setText("tooltip " + item.getText());
            label.addListener(SWT.MouseExit, labelListener);
            label.addListener(SWT.MouseDown, labelListener);
            Point size = tip.computeSize(SWT.DEFAULT, SWT.DEFAULT);
            Rectangle rect = item.getBounds(0);
            Point pt = table.toDisplay(rect.x, rect.y);
            tip.setBounds(pt.x, pt.y, size.x, size.y);
            tip.setVisible(true);
          }
        }
        }
      }
    };
    table.addListener(SWT.Dispose, tableListener);
    table.addListener(SWT.KeyDown, tableListener);
    table.addListener(SWT.MouseMove, tableListener);
    table.addListener(SWT.MouseHover, tableListener);
    shell.open();
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch())
        display.sleep();
    }
    display.dispose();
  }
}