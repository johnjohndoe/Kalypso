package de.tuhh.wb.javagis.view.tableview;

import java.awt.Color;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
import javax.swing.JTable;

import com.bce.datacenter.kalypso.dcbrowser.DCBrowser;
import de.tuhh.wb.javagis.data.TSLink;

public class BCEChooser {

	public BCEChooser() {

	}

	public static void setUpBCEChooser(JTable table) {

		//Set up renderer and editor for the Favorite Date column.

		setUpBCERenderer(table);

		final JButton button = new JButton("");

		button.setBackground(Color.white);

		button.setBorderPainted(false);

		button.setMargin(new Insets(0, 0, 0, 0));

		final TableCellEditorBCE bceEditor = new TableCellEditorBCE(button);

		table.setDefaultEditor(TSLink.class, bceEditor);

		setUpBCEEditor(bceEditor, button);

	}

	private static void setUpBCERenderer(JTable table) {

		table.setDefaultRenderer(TSLink.class, new TableCellRendererBce());

	}

	public static void setUpBCEEditor(
		final TableCellEditorBCE bceEditor,
		final JButton button) {

		

		ActionListener okListener = new ActionListener() {

			public void actionPerformed(ActionEvent e) {

				bceEditor.selectedLink =
					DCBrowser.getInstance().getSelectedNode();
				bceEditor.stopCellEditing();
				System.out.println(
					"selectedNode: " + bceEditor.selectedLink.toString());
			}

		};

		ActionListener cancelListener = new ActionListener() {

			public void actionPerformed(ActionEvent e) {

				bceEditor.cancelCellEditing();

			}

		};

		ActionListener clearListener = new ActionListener() {

			public void actionPerformed(ActionEvent e) {

				bceEditor.selectedLink = new TSLink(null);
				bceEditor.stopCellEditing();

			}

		};

		DCBrowser.getInstance().setActionListener(
			okListener,
			cancelListener,
			clearListener);

		DCBrowser.getInstance().addWindowListener(new WindowAdapter() {

			public void windowClosing(WindowEvent e) {

				bceEditor.cancelCellEditing();

				DCBrowser.getInstance().hide();

			}

		});

		//Here's the code that brings up the dialog.

		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {

				if (bceEditor.selectedLink != null) {

					button.setText(bceEditor.selectedLink.getText());

				} else
					button.setText("");
					
				DCBrowser.getInstance().setVisible(true);
				DCBrowser.getInstance().setLocationRelativeTo(button);
				if (bceEditor.selectedLink != null) {
					try{
					DCBrowser.getInstance().expandTo(
						bceEditor.selectedLink.getCode());
					}
					catch(Exception exeption)
					{
					}
				}
				DCBrowser.getInstance().show();
				DCBrowser.getInstance().toFront();

			}

		});

	}

}
