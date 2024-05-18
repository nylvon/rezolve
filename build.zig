const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "ReZolve",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    const exe = b.addExecutable(.{
        .name = "ReZolve",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const utils_tests = b.addTest(.{
        .root_source_file = b.path("src/utils.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_utils_tests = b.addRunArtifact(utils_tests);

    const autounion_tests = b.addTest(.{
        .root_source_file = b.path("src/autounion.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_autounion_tests = b.addRunArtifact(autounion_tests);

    const node_tests = b.addTest(.{
        .root_source_file = b.path("src/node.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_node_tests = b.addRunArtifact(node_tests);

    const control_tests = b.addTest(.{
        .root_source_file = b.path("src/control.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_control_tests = b.addRunArtifact(control_tests);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const utils_test_step = b.step("test-utils", "Run utils tests");
    utils_test_step.dependOn(&run_utils_tests.step);

    const autounion_test_step = b.step("test-autounion", "Run auto-union tests");
    autounion_test_step.dependOn(&run_utils_tests.step);
    autounion_test_step.dependOn(&run_autounion_tests.step);

    const node_test_step = b.step("test-node", "Run node tests");
    node_test_step.dependOn(&run_utils_tests.step);
    node_test_step.dependOn(&run_autounion_tests.step);
    node_test_step.dependOn(&run_node_tests.step);

    const control_test_step = b.step("test-control", "Run control tests");
    control_test_step.dependOn(&run_utils_tests.step);
    control_test_step.dependOn(&run_autounion_tests.step);
    control_test_step.dependOn(&run_node_tests.step);
    control_test_step.dependOn(&run_control_tests.step);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(utils_test_step);
    test_step.dependOn(autounion_test_step);
    test_step.dependOn(node_test_step);
    test_step.dependOn(control_test_step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
