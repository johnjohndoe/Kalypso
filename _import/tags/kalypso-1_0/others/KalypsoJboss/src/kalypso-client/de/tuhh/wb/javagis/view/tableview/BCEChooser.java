package de.tuhh.wb.javagis.view.tableview;

import javax.swing.JTable;
import javax.swing.JButton;

import java.awt.*;
import java.awt.event.*;

import de.tuhh.wb.javagis.data.TSLink;
import timeserieSelection.CSelectTSFrame;

import de.tuhh.wb.javagis.data.GisElementClass;
import de.tuhh.wb.javagis.data.Version;

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

	private static CSelectTSFrame tsFrameDialog =
		new CSelectTSFrame();

	public static void setUpBCEEditor(
		final TableCellEditorBCE bceEditor,
		final JButton button) {

		ActionListener okListener = new ActionListener() {

			public void actionPerformed(ActionEvent e) {

				bceEditor.selectedLink = tsFrameDialog.getSelectedNode();
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

		tsFrameDialog.setActionListener(
			okListener,
			cancelListener,
			clearListener);

		tsFrameDialog.addWindowListener(new WindowAdapter() {

			public void windowClosing(WindowEvent e) {

				bceEditor.cancelCellEditing();

				tsFrameDialog.hide();

			}

		});

		//Here's the code that brings up the dialog.

		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {

				if (bceEditor.selectedLink != null) {

					button.setText(bceEditor.selectedLink.toString());

				} else
					button.setText("");

				tsFrameDialog.setLocationRelativeTo(button);
				tsFrameDialog.prepareWindow(bceEditor.selectedLink);
				tsFrameDialog.show();
				tsFrameDialog.toFront();

			}

		});

	}

}
